//! Checker methods grouped by responsibility: synthesize spawn forms.
//! Split from `expressions.rs`: checker methods, part 1 of 5.
#![allow(
    unused_imports,
    redundant_imports,
    clippy::wildcard_imports,
    reason = "chunk files share the parent module's import header"
)]
use super::super::branch_join::BranchArmExit;
use super::super::coerce::{cast_is_valid, common_integer_type, common_numeric_type};
use super::super::types::GenericLambdaSig;
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::super::*;
use super::*;
use crate::check::types::{
    DeferredIsCheck, EqRequirement, GenericCallEdge, GenericCallee, GenericFnInstantiationSite,
    PendingInstantiation,
};
use crate::env::{PlaceConflict, PlacePath};
use crate::BuiltinType;
use std::collections::VecDeque;

impl Checker {
    pub(in crate::check) fn report_invalid_actor_send(&mut self, ty: &Ty, span: &Span) {
        self.report_error_with_suggestions(
            TypeErrorKind::InvalidSend,
            span,
            format!(
                "cannot send `{}` to actor: type is not Send",
                ty.user_facing()
            ),
            vec!["keep the resource inside one owning actor and send that actor a message instead — see the language guide, 'Own a resource with an actor'".to_string()],
        );
    }

    /// A resource destructor owns its receiver and may transfer one field to
    /// the external release operation. Close-body cleanup retains every field
    /// it does not move out. This exception is deliberately narrower than an
    /// arbitrary consuming method: it requires the registered inherent
    /// `close(consume self)` contract and the lexical receiver binding.
    pub(super) fn resource_close_owns_self_field(&self, root: &str, parent: &Ty) -> bool {
        if root != "self" {
            return false;
        }
        let Some(function) = self.current_function.as_ref() else {
            return false;
        };
        let Some(signature) = self.fn_sig(function) else {
            return false;
        };
        if !signature.consumes_receiver
            || !signature
                .impl_method
                .as_ref()
                .is_some_and(|method| method.is_inherent && method.name == "close")
        {
            return false;
        }
        matches!(parent, Ty::Named { head, .. } if self.registry.is_resource(head.registry_key()))
    }

    /// Whether a MODULE-QUALIFIED type name denotes a transferring builtin.
    ///
    /// A source-declared lifecycle type (`std.link_monitor.MonitorRef`) reaches
    /// some positions — notably a declared actor state-field type — spelled by
    /// its qualified path with no `builtin` tag attached, so the tag test alone
    /// misses it and the handle silently stayed shareable.
    ///
    /// The qualification requirement is load-bearing: `lookup_builtin_type`
    /// also resolves BARE canonical names, and a user `type MonitorRef` shadow
    /// is a clone-total record that must keep ordinary value semantics. Only
    /// the dotted spelling is the stdlib declaration.
    pub(super) fn qualified_name_resolves_to_transferring_builtin(name: &str) -> bool {
        name.contains('.')
            && crate::lookup_builtin_type(name)
                .is_some_and(BuiltinType::transfers_ownership_across_actor_boundary)
    }

    pub(in crate::check) fn check_field_access(
        &mut self,
        object: &Spanned<Expr>,
        field: &str,
        span: &Span,
    ) -> Ty {
        self.check_field_access_with_type_args(object, field, None, span)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "field access handles many type variants"
    )]
    pub(super) fn check_field_access_with_type_args(
        &mut self,
        object: &Spanned<Expr>,
        field: &str,
        type_args: Option<&[Spanned<TypeExpr>]>,
        span: &Span,
    ) -> Ty {
        if type_args.is_some()
            && matches!(&object.0, Expr::Ident(name) if self.env.lookup_ref(name.name.as_str()).is_some())
        {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "explicit type arguments require a function declaration, not a value field"
                    .to_string(),
            );
            return Ty::Error;
        }

        // `self.count` is the receiver spelling of the actor state binding
        // `count`. Delegate to the bare-name shell so the read gets the same
        // type, the same use-after-move reporting, and the same HIR binding
        // reference the bare spelling gets at this site.
        if let Some(state_field) = self.actor_self_state_field(&object.0, field) {
            self.record_actor_self_state_field(span);
            return self.synthesize_identifier(state_field, span);
        }
        if self.is_actor_self_receiver(&object.0) {
            let similar = crate::error::find_similar(
                field,
                self.current_actor_fields.iter().map(|f| f.name.as_str()),
            );
            self.report_error_with_suggestions(
                TypeErrorKind::UndefinedField,
                span,
                format!("actor state has no field `{field}`"),
                similar,
            );
            return Ty::Error;
        }
        if let Some(head) = self.resolve_dotted_type_head(object, field) {
            if let Some(result) = self.dispatch_dotted_type_member(
                &head,
                field,
                &DottedTypeMemberUse::Reference { span },
            ) {
                self.mark_resolved_nominal_owner_used(&head.canonical_type);
                return result;
            }
        }

        // Dotted type members were dispatched from the canonical head above.
        // Remaining identifiers are ordinary value projections or unresolved
        // names and continue through the existing diagnostics.

        // Dotted module-qualified unit constructor:
        // `module.Type.Variant`. The parser represents this as nested field
        // access, but neither `module` nor `module.Type` is a runtime value.
        // Resolve the complete constructor before synthesising the inner
        // projection so it shares the exact export and variant authority of
        // the existing `module.Type::Variant` surface.
        if let Expr::FieldAccess {
            object: module,
            field: type_name,
        } = &object.0
        {
            if let Expr::Ident(module_short) = &module.0 {
                if self.module_binding_in_current_file(module_short.name.as_str())
                    && self.env.lookup_ref(module_short.name.as_str()).is_none()
                {
                    let constructor = format!("{module_short}.{}::{}", type_name.0, field);
                    return self.synthesize_identifier(&constructor, span);
                }
            }
        }

        // Pre-dispatch: module-qualified value-constructor reference, e.g.
        // `m.Type::Variant` (unit or tuple-naked).  This must run BEFORE
        // `synthesize(object)` because `module` is not bound in `self.env`
        // — without the early dispatch the synthesize call would emit the
        // leaky "undefined variable `module`" diagnostic.
        //
        // Mirrors the `module_fn_exports` guard pattern at
        // `check_method_call` (methods.rs).  Gated on:
        //   - object is a bare `Expr::Ident`
        //   - `field` contains `::` (the type-variant separator)
        //   - the identifier is neither a value binding nor a known type
        // The neither-binding-nor-type guard preserves all existing
        // field-on-value access semantics — only shapes that could only be a
        // module-qualified reference take the new path.  Nested-module paths
        // (`a.b.Type::Variant`) are out of scope for v0.5.
        if let Expr::Ident(name) = &object.0 {
            if let Some(pos) = field.find("::") {
                let receiver_is_binding = self.env.lookup_ref(name.name.as_str()).is_some();
                let receiver_is_known_type = self.type_def_at(name.name.as_str());
                if !receiver_is_binding && receiver_is_known_type.is_none() {
                    let type_name = &field[..pos];
                    let variant_name = &field[pos + 2..];
                    return self.check_module_qualified_variant_ref(
                        name.name.as_str(),
                        type_name,
                        variant_name,
                        span,
                    );
                }
            }
        }

        // Pre-dispatch: module-qualified constant reference, e.g. `module.CONST_NAME`.
        // Must run BEFORE `synthesize(object)` for the same reason as the variant
        // arm above — the module short-name is not in env as a value binding.
        //
        // Gated on:
        //   - object is a bare `Expr::Ident`
        //   - field does NOT contain `::` (plain const name, not a variant)
        //   - receiver is not a value binding or known type
        //   - the lexical module binding resolves to an exact owner-qualified
        //     constant key registered in env
        if let Expr::Ident(name) = &object.0 {
            if !field.contains("::") {
                let receiver_is_binding = self.env.lookup_ref(name.name.as_str()).is_some();
                let receiver_is_known_type = self.type_def_at(name.name.as_str());
                if !receiver_is_binding && receiver_is_known_type.is_none() {
                    let lexical_key = format!("{name}.{field}");
                    let qualified_key = self
                        .module_import_bindings
                        .get(&(
                            self.current_module.clone(),
                            self.current_module_idx,
                            name.to_string(),
                        ))
                        .map_or_else(|| lexical_key.clone(), |owner| format!("{owner}.{field}"));
                    if let Some(binding) = self.env.lookup_ref(&qualified_key) {
                        let ty = binding.ty.clone();
                        if self.module_binding_in_current_file(name.name.as_str()) {
                            self.used_modules.borrow_mut().insert(ImportKey::in_file(
                                self.current_module.clone(),
                                self.current_module_idx,
                                name.to_string(),
                            ));
                        }
                        return ty;
                    }
                    // If the receiver looks like a module (known to self.modules) but
                    // the const is not exported, emit a targeted diagnostic rather than
                    // falling through to the generic "undefined variable `module`" error.
                    if self.module_binding_in_current_file(name.name.as_str()) {
                        if self.has_fn_sig(&qualified_key) {
                            self.used_modules.borrow_mut().insert(ImportKey::in_file(
                                self.current_module.clone(),
                                self.current_module_idx,
                                name.to_string(),
                            ));
                            self.reject_wasm_native_only_module_function(
                                name.name.as_str(),
                                field,
                                span,
                            );
                            if self.is_shipped_crypto_module(name.name.as_str())
                                && matches!(field, "random_bytes" | "try_random_bytes")
                            {
                                self.reject_wasm_feature(
                                    span,
                                    WasmUnsupportedFeature::CryptoRandom,
                                );
                            }
                            self.record_call_edge(&qualified_key);
                            return self.instantiate_function_value(
                                &qualified_key,
                                type_args,
                                span,
                            );
                        }
                        let similar = crate::error::find_similar(
                            field,
                            self.env
                                .all_names()
                                .map(Symbol::as_str)
                                .filter_map(|k| k.strip_prefix(&format!("{name}.")))
                                .filter(|k| !k.contains('.')),
                        );
                        if self
                            .resolve_module_type(name.name.as_str(), field)
                            .is_some()
                        {
                            self.report_error(
                                TypeErrorKind::PathKindMismatch,
                                span,
                                format!("module member `{name}.{field}` is a type, not a value"),
                            );
                            return Ty::Error;
                        }
                        self.report_error_with_suggestions(
                            TypeErrorKind::PathMemberNotFound,
                            span,
                            format!("module `{name}` has no exported value `{field}`"),
                            similar,
                        );
                        return Ty::Error;
                    }
                }
            }
        }

        // The object is the BASE of this projection, not a whole-value use of
        // itself: `h.other` stays legal after `h.sock` moved out.
        self.place_base_depth += 1;
        let obj_ty = self.synthesize(&object.0, &object.1);
        self.place_base_depth -= 1;
        // Reading this projection after it (or storage under it) was consumed
        // is a use-after-move. Assignment targets are exempt: the outermost
        // target place is written, not read.
        if self.place_write_depth == 0 || self.place_base_depth > 0 {
            if let Some((root, mut path)) = self.expr_place(&object.0) {
                path.push(field.to_string());
                self.report_place_use_after_move(&root, &path, span);
            }
        }
        let resolved = self.normalize_for_use(&obj_ty);
        if self.reject_sealed_delivery_access(&resolved, span) {
            return Ty::Error;
        }

        match &resolved {
            // `Range<T>` exposes its bounds as `start`/`end`. It carries no
            // `TypeDef` (it is a compiler builtin, not a user declaration), so
            // the two fields resolve straight from the type's own argument
            // instead of the `type_defs` table the generic `Named` arm below
            // reads from. Any other field name falls through to that arm,
            // finds no `TypeDef` for `Range`, and reports `UndefinedField`
            // exactly as before.
            Ty::Named {
                head: crate::TypeHead::Builtin(BuiltinType::Range),
                args,
                ..
            } if args.len() == 1 && matches!(field, "start" | "end") => args[0].clone(),
            Ty::Named { head, args } => {
                let name = head.registry_key();
                // A role retains the child's complete type after substituting
                // the owning supervisor's concrete arguments.
                if let Some(Ty::Named {
                    head: sup_head,
                    args: sup_args,
                }) = resolved.as_local_actor_ref()
                {
                    if let Some(children) = self
                        .supervisor_children
                        .get(sup_head.registry_key())
                        .cloned()
                    {
                        let selected = children
                            .statics
                            .iter()
                            .enumerate()
                            .map(|(index, child)| (super::types::ChildKind::Static, index, child))
                            .chain(children.pools.iter().enumerate().map(|(index, child)| {
                                (super::types::ChildKind::Pool, index, child)
                            }))
                            .find(|(_, _, (name, _))| name == field);
                        if let Some((kind, index, (child_name, template))) = selected {
                            let parameters = self
                                .type_def_at(sup_head.registry_key())
                                .map_or_else(Vec::new, |definition| definition.type_params.clone());
                            let substitution = parameters
                                .into_iter()
                                .zip(sup_args.iter().cloned())
                                .collect();
                            let child_ty = template.substitute_named_params_parallel(&substitution);
                            if let Ty::Named { head, args } = &child_ty {
                                self.enforce_type_def_instantiation_bounds(
                                    head.registry_key(),
                                    args,
                                    span,
                                );
                            }
                            self.supervisor_child_slots.insert(
                                SpanKey::in_module(span, self.current_module_idx),
                                super::types::ChildSlot {
                                    kind,
                                    index: u32::try_from(index)
                                        .expect("supervisor child count exceeds u32"),
                                    child_ty: child_ty.user_facing().to_string(),
                                    child_name: child_name.clone(),
                                    supervisor: sup_head.registry_key().to_string(),
                                },
                            );
                            if kind == super::types::ChildKind::Pool {
                                return Ty::supervisor_pool(
                                    Ty::actor_handle(
                                        match sup_head {
                                            crate::TypeHead::Nominal(nominal)
                                            | crate::TypeHead::Actor(nominal) => *nominal,
                                            _ => unreachable!(
                                                "a local actor reference names an actor"
                                            ),
                                        },
                                        sup_args.clone(),
                                    ),
                                    child_ty,
                                );
                            }
                            return Ty::child_ref(child_ty);
                        }
                        let names = children
                            .statics
                            .iter()
                            .chain(children.pools.iter())
                            .map(|(name, _)| name.as_str());
                        let similar = crate::error::find_similar(field, names);
                        self.report_error_with_suggestions(
                            TypeErrorKind::UndefinedField,
                            span,
                            format!(
                                "supervisor `{}` has no child named `{field}`",
                                sup_head.registry_key()
                            ),
                            similar,
                        );
                        return Ty::Error;
                    }
                }
                if let Some(td) = self.lookup_type_def(name) {
                    if let Some(field_ty) = td.fields.get(field) {
                        // Substitute generic type params with concrete args in
                        // parallel so a swap instantiation like `Pair<B, A>` does
                        // not alias: sequential A→B then B→A would produce A again.
                        let subst_map: HashMap<String, Ty> = td
                            .type_params
                            .iter()
                            .zip(args.iter())
                            .map(|(p, a)| (p.clone(), a.clone()))
                            .collect();
                        field_ty.substitute_named_params_parallel(&subst_map)
                    } else {
                        let similar =
                            crate::error::find_similar(field, td.fields.keys().map(String::as_str));
                        self.report_error_with_suggestions(
                            TypeErrorKind::UndefinedField,
                            span,
                            format!("no field `{field}` on type `{name}`"),
                            similar,
                        );
                        Ty::Error
                    }
                } else {
                    self.report_error(
                        TypeErrorKind::UndefinedField,
                        span,
                        format!(
                            "cannot access field `{field}` on `{}`",
                            resolved.user_facing()
                        ),
                    );
                    Ty::Error
                }
            }
            Ty::Tuple(elems) => {
                // Tuple field access by index: t.0, t.1
                if let Ok(idx) = field.parse::<usize>() {
                    if idx < elems.len() {
                        elems[idx].clone()
                    } else {
                        self.report_error(
                            TypeErrorKind::UndefinedField,
                            span,
                            format!("tuple index {idx} out of range (len {})", elems.len()),
                        );
                        Ty::Error
                    }
                } else {
                    Ty::Error
                }
            }
            _ => {
                if resolved != Ty::Error {
                    self.report_error(
                        TypeErrorKind::UndefinedField,
                        span,
                        format!(
                            "cannot access field `{field}` on `{}`",
                            resolved.user_facing()
                        ),
                    );
                }
                Ty::Error
            }
        }
    }

    pub(in crate::check) fn check_match_expr(
        &mut self,
        scrutinee_ty: &Ty,
        scrutinee: &Spanned<Expr>,
        arms: &[MatchArm],
        span: &Span,
        expected: Option<&Ty>,
    ) -> Ty {
        if arms.is_empty() {
            let resolved = self.subst.resolve(scrutinee_ty);
            let uninhabited = match &resolved {
                Ty::Never => true,
                Ty::Named { head, .. } => {
                    self.lookup_type_def(head.registry_key())
                        .is_some_and(|definition| {
                            definition.kind == TypeDefKind::Enum && definition.variants.is_empty()
                        })
                }
                _ => false,
            };
            if uninhabited {
                return Ty::Never;
            }
            if resolved != Ty::Error {
                self.report_error(
                    TypeErrorKind::NonExhaustiveMatch,
                    span,
                    format!(
                        "an empty match cannot cover inhabited type `{}`",
                        resolved.user_facing()
                    ),
                );
            }
            return Ty::Error;
        }

        let scrutinee_place = self.expr_place(&scrutinee.0);
        let scrutinee_loan = self.collection_borrow_origin(&scrutinee.0, &scrutinee.1);
        // If the enclosing context supplies a concrete expected type (e.g. the
        // function's declared return type), pre-seed result_ty so every arm body
        // is checked with check_against rather than having the first arm's
        // synthesized type (which defaults literals to i64) propagate to later arms.
        let resolved_expected = expected.map(|ty| self.subst.resolve(ty));
        let mut result_ty: Option<Ty> = match &resolved_expected {
            Some(ty) if !matches!(ty, Ty::Var(_) | Ty::Error) => Some(ty.clone()),
            _ => None,
        };
        // When this `match` is itself a function-return tail, every arm body
        // flows to the return and may Ok-coerce. Capture the armed state once;
        // the per-arm guard check and pattern binding are not tail positions, so
        // re-arm immediately before each arm body.
        let tail_ok_armed = std::mem::replace(&mut self.tail_ok_armed, false);
        // Exactly one arm BODY runs, so each body starts from the ownership
        // state at the match's entry rather than from whatever the previous arm
        // left behind, and the state after the match is the union over the arms
        // that actually reach the join.
        //
        // Guards are not bodies. A guard runs whenever its pattern matched and
        // every earlier arm did not, so guard N and body N+1 both execute on one
        // path. Guards therefore thread through a running fall-through state —
        // the same treatment an `else if` chain's conditions get — and each body
        // starts from the fall-through its own guard produced.
        //
        // A guard that DIVERGES is the exception, and it cuts both ways. A later
        // arm is reached only when this arm's pattern failed, and then the guard
        // never ran at all — so a diverging guard contributes nothing to the
        // fall-through. Its own body is unreachable for the same reason, so the
        // body's exit must stay out of the join no matter what the body does.
        let ownership_entry = self.env.ownership_snapshot();
        let mut fall_through = ownership_entry.clone();
        let mut arm_exits = Vec::with_capacity(arms.len());
        for arm in arms {
            self.env.push_scope();
            self.env.restore_ownership(&fall_through);
            self.bind_scrutinee_pattern(
                &arm.pattern,
                scrutinee_ty,
                false,
                scrutinee_place.clone(),
                scrutinee_loan.clone(),
            );
            self.record_arm_resolution(&arm.pattern.0, &arm.pattern.1, scrutinee_ty);

            let mut guard_diverges = false;
            if let Some((guard, gs)) = &arm.guard {
                // Pattern bindings borrow during candidate testing. Their
                // field transfers happen only after the guard selects this
                // arm, so a declined candidate cannot move the source.
                let pattern_entry = fall_through.clone();
                let selected_pattern = self.env.ownership_snapshot();
                self.env.restore_ownership(&fall_through);
                let guard_ty = self.check_against(guard, gs, &Ty::Bool);
                if Self::arm_skips_join(&guard_ty) {
                    guard_diverges = true;
                    // Rewind the guard's consumes: neither the unreachable body
                    // below nor any later arm ever observes them.
                    self.env.restore_ownership(&fall_through);
                } else {
                    // The guard ran and returned false; later arms see its state.
                    fall_through = self.env.ownership_snapshot();
                    self.env
                        .apply_pattern_moves(&pattern_entry, &selected_pattern);
                }
            }

            self.tail_ok_armed = tail_ok_armed;
            let arm_ty = if let Some(expected) = &result_ty {
                if expected.contains_callable() && resolved_expected.is_none() {
                    self.synthesize(&arm.body.0, &arm.body.1)
                } else {
                    self.check_expr_with_expected(&arm.body.0, &arm.body.1, expected)
                }
            } else {
                self.synthesize(&arm.body.0, &arm.body.1)
            };
            self.record_value_transfer(&arm.body.0, &arm.body.1);
            arm_exits.push(BranchArmExit {
                ownership: self.env.ownership_snapshot(),
                diverges: guard_diverges || Self::arm_skips_join(&arm_ty),
            });
            // Skip Never/Error when setting the expected type — diverging arms
            // (return, panic, break) shouldn't constrain the match result type.
            if !matches!(arm_ty, Ty::Never | Ty::Error) {
                result_ty = Some(if let Some(previous) = result_ty {
                    if previous.contains_callable() || arm_ty.contains_callable() {
                        self.unify_branches(&previous, &arm_ty, span)
                    } else {
                        previous
                    }
                } else {
                    arm_ty
                });
            }

            self.env.pop_scope();
        }
        self.join_branch_ownership(&ownership_entry, &arm_exits);
        // Leave the flag disarmed: the arm loop set it per-arm, and the
        // exhaustiveness check below is not a tail position.
        self.tail_ok_armed = false;

        // Exhaustiveness check for enums/Option/Result
        self.check_exhaustiveness(scrutinee_ty, arms, span);

        // If all arms diverge (Never/Error), the match itself diverges
        result_ty.unwrap_or(Ty::Never)
    }

    #[expect(
        clippy::too_many_arguments,
        reason = "lambda checking combines contextual inference with capture analysis"
    )]
    pub(in crate::check) fn check_lambda(
        &mut self,
        is_move: bool,
        private_captures: &[Spanned<Ident>],
        type_params: Option<&[TypeParam]>,
        params: &[LambdaParam],
        return_type: Option<&Spanned<TypeExpr>>,
        body: &Spanned<Expr>,
        expected: Option<(&[Ty], &Ty)>,
        span: &Span,
        is_actor_body: bool,
        is_fork_body: bool,
    ) -> Ty {
        let key = SpanKey::in_module(span, self.current_module_idx);
        let owner = super::effects::EffectBody::Closure(key.clone());
        self.effect_graph.bodies.entry(owner.clone()).or_default();
        let previous = self.effect_graph.current_body.replace(owner);
        let result = self.check_lambda_body(
            is_move,
            private_captures,
            type_params,
            params,
            return_type,
            body,
            expected,
            span,
            is_actor_body,
            is_fork_body,
        );
        self.effect_graph.current_body = previous;
        result
    }

    #[expect(
        clippy::too_many_arguments,
        clippy::too_many_lines,
        reason = "lambda checking combines contextual inference with capture analysis"
    )]
    pub(super) fn check_lambda_body(
        &mut self,
        is_move: bool,
        private_captures: &[Spanned<Ident>],
        type_params: Option<&[TypeParam]>,
        params: &[LambdaParam],
        return_type: Option<&Spanned<TypeExpr>>,
        body: &Spanned<Expr>,
        expected: Option<(&[Ty], &Ty)>,
        span: &Span,
        is_actor_body: bool,
        is_fork_body: bool,
    ) -> Ty {
        let private_bindings = self.resolve_private_captures(private_captures);
        let body_environment = self
            .env
            .closure_environment(&private_bindings, is_actor_body);
        let outer_environment = std::mem::replace(&mut self.env, body_environment);
        // Save/restore capture tracking state for nested lambdas
        let prev_capture_depth = self.lambda_capture_depth;
        let prev_captures = std::mem::take(&mut self.lambda_captures);
        let prev_capture_facts = std::mem::take(&mut self.lambda_capture_facts);
        let prev_actor_handler_context = self.in_actor_handler_context;
        self.in_actor_handler_context = false;
        // A lambda body does not inherit the lexical task scope it is written
        // inside: the closure may run after the scope has joined, so `fork`
        // statements inside it have no spawn context.
        let prev_task_scope_depth = self.task_scope_depth;
        self.task_scope_depth = 0;
        let prev_in_lambda_actor_body = self.in_lambda_actor_body;
        // Set is_actor_body for the duration of this lambda's body; nested fn-closures
        // are called with is_actor_body=false, so they get false regardless of the outer flag.
        self.in_lambda_actor_body = is_actor_body;

        // Record the scope depth BEFORE pushing the lambda scope — any variable
        // found below this depth during body checking is a capture.
        let capture_depth = self.env.depth();
        self.lambda_capture_depth = Some(capture_depth);

        // Clear any stale scratch state from a previous call in a non-let or
        // nested context.  We unconditionally reset first so that re-entrant
        // calls (e.g., a generic lambda inside a function argument) cannot
        // bleed their type-var pairs out to an unrelated enclosing Stmt::Let.
        self.last_lambda_generic_sig = None;

        let mut generic_bindings = std::collections::HashMap::new();
        let mut generic_param_names = HashMap::new();
        let mut generic_type_vars = Vec::new();
        if let Some(tps) = type_params {
            for tp in tps {
                let tv = TypeVar::fresh();
                generic_bindings.insert(tp.name.to_string(), Ty::Var(tv));
                generic_param_names.insert(tv.0, tp.name.to_string());
                generic_type_vars.push(tv);
            }
        }
        if !generic_bindings.is_empty() {
            self.generic_ctx.push(generic_bindings);
        }

        self.env.push_scope();
        let prev_in_generator = self.in_generator;
        self.in_generator = false;

        // Check arity mismatch: lambda parameter count must match expected function type
        if let Some((expected_params, _)) = &expected {
            if params.len() != expected_params.len() {
                self.errors.push(TypeError::new(
                    TypeErrorKind::ArityMismatch,
                    span.clone(),
                    format!(
                        "lambda has {} parameters but expected function type has {}",
                        params.len(),
                        expected_params.len()
                    ),
                ));
            }
        }

        let mut param_tys = Vec::new();
        for (i, p) in params.iter().enumerate() {
            let ty = if let Some(annotation) = &p.ty {
                let (annotated_ty, hole_vars) = self.resolve_annotation_holes(annotation);
                // Unify the annotated type against the expected param type regardless
                // of whether the annotation contains holes.  The holes path (deferred
                // inference) is orthogonal: a fully-concrete annotation (`|x: i64|`)
                // must still be rejected when the expected param type is `bool`.
                if let Some((expected_params, _)) = &expected {
                    if let Some(expected_ty) = expected_params.get(i) {
                        self.expect_type(expected_ty, &annotated_ty, &annotation.1);
                    }
                }
                if !hole_vars.is_empty() {
                    self.record_deferred_inference_holes(
                        annotation,
                        format!("lambda parameter `{}`", p.name),
                        hole_vars,
                    );
                }
                self.subst.resolve(&annotated_ty)
            } else if let Some((expected_params, _)) = &expected {
                expected_params
                    .get(i)
                    .cloned()
                    .unwrap_or_else(|| Ty::Var(TypeVar::fresh()))
            } else {
                Ty::Var(TypeVar::fresh())
            };
            self.check_shadowing(p.name.name.as_str(), &p.name_span);
            self.env.define_param_with_span(
                p.name.to_string(),
                ty.clone(),
                false,
                p.name_span.clone(),
            );
            param_tys.push(ty);
        }

        // Save enclosing return type and install the lambda's own return type so
        // that PostfixTry (`?`) context checks see the lambda's return type,
        // not the outer function's.
        let prev_return_type = self.current_return_type.take();
        let previous_defer = self.deferred_body.take();
        let prev_fails = std::mem::replace(&mut self.current_fails, false);

        let previous_inferred_returns = self.inferred_lambda_returns.take();
        let ret_ty = if let Some(annotation) = return_type {
            let (expected_ret, hole_vars) = self.resolve_annotation_holes(annotation);
            // Unify the annotated return type against the contextual expected return
            // type regardless of holes — same rationale as annotated param types above.
            if let Some((_, contextual_ret)) = expected {
                self.expect_type(contextual_ret, &expected_ret, &annotation.1);
            }
            if !hole_vars.is_empty() {
                self.record_deferred_inference_holes(annotation, "lambda return type", hole_vars);
            }
            self.current_return_type = Some(expected_ret.clone());
            // Guard: do not pre-seed body with Ty::Error (unresolvable annotation).
            // Synthesize instead so internal body errors are still reported.
            let resolved_ret = self.subst.resolve(&expected_ret);
            if matches!(resolved_ret, Ty::Error) {
                self.synthesize(&body.0, &body.1);
            } else {
                self.check_against(&body.0, &body.1, &expected_ret);
            }
            self.subst.resolve(&expected_ret)
        } else if let Some((_, expected_ret)) = expected {
            self.current_return_type = Some(expected_ret.clone());
            self.check_against(&body.0, &body.1, expected_ret);
            expected_ret.clone()
        } else {
            self.infer_lambda_result(body)
        };
        self.inferred_lambda_returns = previous_inferred_returns;
        self.record_value_transfer(&body.0, &body.1);

        self.current_return_type = prev_return_type;
        self.deferred_body = previous_defer;
        self.current_fails = prev_fails;
        self.in_actor_handler_context = prev_actor_handler_context;
        self.task_scope_depth = prev_task_scope_depth;
        self.in_lambda_actor_body = prev_in_lambda_actor_body;
        self.in_generator = prev_in_generator;
        self.env.pop_scope();

        if let Some(tps) = type_params {
            if !tps.is_empty() {
                let type_param_bounds = tps
                    .iter()
                    .filter_map(|tp| {
                        if tp.bounds.is_empty() {
                            None
                        } else {
                            Some((
                                tp.name.to_string(),
                                tp.bounds
                                    .iter()
                                    .map(|bound| bound.path.to_string())
                                    .collect(), // TRANSITION(P1): deleted by A1 commit 2
                            ))
                        }
                    })
                    .collect();
                self.last_lambda_generic_sig = Some(GenericLambdaSig {
                    call_sig: FnSig {
                        type_params: tps.iter().map(|tp| tp.name.to_string()).collect(),
                        type_param_bounds,
                        param_names: params.iter().map(|param| param.name.to_string()).collect(),
                        params: param_tys
                            .iter()
                            .map(|param| {
                                Self::lambda_generic_schema_ty(param, &generic_param_names)
                            })
                            .collect(),
                        return_type: Self::lambda_generic_schema_ty(&ret_ty, &generic_param_names),
                        ..FnSig::default()
                    },
                    type_vars: generic_type_vars,
                });
                self.generic_ctx.pop();
            }
        }

        let body_environment = std::mem::replace(&mut self.env, outer_environment);
        self.env.merge_closure_reads(&body_environment);
        let raw_capture_facts = std::mem::take(&mut self.lambda_capture_facts);
        // Acquisition happens in the enclosing scope, not in the new closure.
        self.lambda_capture_depth = prev_capture_depth;
        let capture_facts = self.finish_closure_captures(
            raw_capture_facts,
            &private_bindings,
            &body_environment,
            is_move,
            span,
            is_fork_body,
        );
        let capabilities = self.closure_capabilities(&capture_facts);
        self.closure_capture_facts.insert(
            SpanKey::in_module(span, self.current_module_idx),
            capture_facts.clone(),
        );

        // The callable payload and its guarantees come from the same resolved captures.
        let captures: Vec<Ty> = capture_facts.iter().map(|fact| fact.ty.clone()).collect();

        // Restore outer capture tracking state
        self.lambda_captures = prev_captures;
        self.lambda_capture_facts = prev_capture_facts;
        if let Some(depth) = prev_capture_depth {
            for fact in &capture_facts {
                if self
                    .env
                    .lookup_with_depth(&fact.name)
                    .is_some_and(|(binding_depth, binding)| {
                        binding_depth < depth && binding.id == fact.binding_id
                    })
                {
                    self.lambda_capture_facts.push(fact.clone());
                }
            }
        }

        // Every literal has a concrete environment type, including an empty one.
        // Callable guarantees do not erase the identity needed by HIR and SIR.
        Ty::Closure {
            capabilities,
            params: param_tys,
            ret: Box::new(ret_ty),
            captures,
            identity: super::effects::EffectBody::Closure(SpanKey::in_module(
                span,
                self.current_module_idx,
            )),
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "struct and enum-variant initialization share one exact-owner diagnostic path"
    )]
    pub(in crate::check) fn check_struct_init(
        &mut self,
        name: &str,
        fields: &[(Ident, Spanned<Expr>)],
        type_args: Option<&[Spanned<TypeExpr>]>,
        base: Option<&Spanned<Expr>>,
        span: &Span,
    ) -> Ty {
        // Every field is initialized exactly once: a base supplies the fields
        // the literal does not name, and naming one twice leaves no reading
        // that says which value wins.
        let mut named: HashSet<&str> = HashSet::new();
        for (field_name, (_, field_span)) in fields {
            if !named.insert(field_name.name.as_str()) {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    field_span,
                    format!("record literal names field `{field_name}` more than once"),
                );
            }
        }

        // Expression-position struct variants use the final dotted surface
        // (`Type.Variant { ... }`). Normalize that spelling only after the
        // owner has been selected by checker authority: a lexical nominal
        // binding for `Type.Variant`, or the exact export table for
        // `module.Type.Variant`. The resulting registry key retains the full
        // declaration owner and never scans by final segment.
        let dotted_struct_variant = if name.contains("::") {
            None
        } else {
            let segments = name.split('.').collect::<Vec<_>>();
            match segments.as_slice() {
                [surface_type, variant] if self.env.lookup_ref(surface_type).is_none() => self
                    .source_nominal_declaration(surface_type)
                    .and_then(|canonical_type| {
                        self.lookup_type_def(&canonical_type)
                            .filter(|type_def| {
                                matches!(
                                    type_def.variants.get(*variant),
                                    Some(VariantDef::Struct(_))
                                )
                            })
                            .map(|_| format!("{canonical_type}::{variant}"))
                    }),
                [module_short, surface_type, variant]
                    if self.env.lookup_ref(module_short).is_none() =>
                {
                    self.resolve_module_variant(module_short, surface_type, variant)
                        .filter(|(_, variant_def)| matches!(variant_def, VariantDef::Struct(_)))
                        .map(|_| {
                            self.used_modules.borrow_mut().insert(ImportKey::in_file(
                                self.current_module.clone(),
                                self.current_module_idx,
                                (*module_short).to_string(),
                            ));
                            format!(
                                "{}.{surface_type}::{variant}",
                                self.canonical_module_import_owner(module_short)
                            )
                        })
                }
                _ => None,
            }
        };
        // A plain record constructor uses the two-segment `module.Type`
        // surface, which overlaps syntactically with a local
        // `Type.StructVariant`. Give the proven local variant above first
        // refusal, then resolve a lexical module binding through the same
        // export table as annotation-position qualified types. The lexical
        // alias is never a nominal identity: carry the declaration's full
        // source owner into the shared record-initialiser path.
        let module_record_name = if dotted_struct_variant.is_none() && !name.contains("::") {
            let segments = name.split('.').collect::<Vec<_>>();
            match segments.as_slice() {
                [module_short, type_name]
                    if self.env.lookup_ref(module_short).is_none()
                        && self.module_binding_in_current_file(module_short) =>
                {
                    self.used_modules.borrow_mut().insert(ImportKey::in_file(
                        self.current_module.clone(),
                        self.current_module_idx,
                        (*module_short).to_string(),
                    ));
                    let Some(_) = self.resolve_module_type(module_short, type_name) else {
                        let similar = self
                            .module_type_exports_for_binding(module_short)
                            .map(|set| {
                                crate::error::find_similar(
                                    type_name,
                                    set.iter().map(String::as_str),
                                )
                            })
                            .unwrap_or_default();
                        self.report_error_with_suggestions(
                            TypeErrorKind::UndefinedType,
                            span,
                            format!("module `{module_short}` has no exported type `{type_name}`"),
                            similar,
                        );
                        return Ty::Error;
                    };
                    Some(format!(
                        "{}.{type_name}",
                        self.canonical_module_import_owner(module_short)
                    ))
                }
                _ => None,
            }
        } else {
            None
        };
        let name = dotted_struct_variant
            .as_deref()
            .or(module_record_name.as_deref())
            .unwrap_or(name);
        let Ok(canonical_lifecycle_name) =
            self.canonicalize_source_lifecycle_value_path(name, span)
        else {
            return Ty::Error;
        };
        let name = canonical_lifecycle_name.as_deref().unwrap_or(name);

        // Module-qualified diagnostic pre-pass: when `name` has the shape
        // `module.Type::Variant` and `module` is a known module alias, route
        // the failure modes (no exported type / no such variant) through the
        // same fail-closed diagnostics used by `check_field_access`'s
        // module-qualified pre-dispatch.  Without this pre-pass the
        // enum-variant fallback in the main body falls through to
        // "undefined type `module.Type::Variant`" which leaks the
        // qualified-name layout into the diagnostic and gives the user no
        // actionable signal.  Success cases (both type and variant exist) fall
        // through to the existing struct/enum-variant init logic.
        let mut resolved_module_variant_name = None;
        if let Some(dot) = name.find('.') {
            let module_short = &name[..dot];
            if self.module_binding_in_current_file(module_short) {
                let after_dot = &name[dot + 1..];
                if let Some(colon) = after_dot.find("::") {
                    let type_name = &after_dot[..colon];
                    let variant_name = &after_dot[colon + 2..];
                    self.used_modules.borrow_mut().insert(ImportKey::in_file(
                        self.current_module.clone(),
                        self.current_module_idx,
                        module_short.to_string(),
                    ));
                    let Some(td) = self.resolve_module_type(module_short, type_name) else {
                        let similar = self
                            .module_type_exports_for_binding(module_short)
                            .map(|set| {
                                crate::error::find_similar(
                                    type_name,
                                    set.iter().map(String::as_str),
                                )
                            })
                            .unwrap_or_default();
                        self.report_error_with_suggestions(
                            TypeErrorKind::UndefinedType,
                            span,
                            format!("module `{module_short}` has no exported type `{type_name}`"),
                            similar,
                        );
                        return Ty::Error;
                    };
                    if !td.variants.contains_key(variant_name) {
                        let similar = crate::error::find_similar(
                            variant_name,
                            td.variants.keys().map(String::as_str),
                        );
                        self.report_error_with_suggestions(
                            TypeErrorKind::UndefinedField,
                            span,
                            format!(
                                "type `{module_short}.{type_name}` has no variant `{variant_name}`"
                            ),
                            similar,
                        );
                        return Ty::Error;
                    }
                    // Both type and variant exist. Carry the exact declaration
                    // owner into the shared struct-variant path; retaining the
                    // lexical module alias here would leave both its surface
                    // alias and the full declaration as candidates.
                    resolved_module_variant_name = Some(format!(
                        "{}.{type_name}::{variant_name}",
                        self.canonical_module_import_owner(module_short)
                    ));
                }
            }
        }
        let name = resolved_module_variant_name.as_deref().unwrap_or(name);
        // Fail closed under qualified-by-default before binding a bare record
        // constructor: a bare name published by more than one module is
        // ambiguous, and one exported but published by none is not in scope.
        // Without this gate the construction falls through to `lookup_type_def`
        // and silently binds a last-write-wins bare def, then trips a confusing
        // downstream MIR field-order failure. The `::` enum-variant and
        // explicitly module-qualified spellings already routed above are left
        // untouched (they carry a `.` or `::` and never match a bare name).
        let is_bare_constructor = !name.contains('.') && !name.contains("::");
        if is_bare_constructor && self.report_bare_type_scope_error(name, span) {
            return Ty::Error;
        }
        // A bare construction (`Gadget { … }`) of a type published by exactly
        // one imported module binds to that owner's QUALIFIED identity, so the
        // constructed value carries `owner.Gadget` rather than the bare
        // last-write-wins key. This keeps two modules' same-bare-name records
        // from colliding in the downstream MIR record-layout / field-order
        // registry (the same identity discipline `samename_type_layout` proves
        // for explicitly qualified constructions).
        let qualified_owned = self
            .published_bare_type_qualified(name)
            .or_else(|| self.flat_file_import_type_owner(name));
        let delivery_owner = self
            .canonical_nominal_name(name)
            .unwrap_or_else(|| qualified_owned.clone().unwrap_or_else(|| name.to_string()));
        if self.reject_sealed_delivery_access(
            &self.named_ty_for_key(&delivery_owner, Vec::new()),
            span,
        ) {
            return Ty::Error;
        }

        if let Some(qualified) = qualified_owned.as_deref() {
            // `qualified` is the full owner-qualified source identity
            // (`owner.TypeName`), and `owner` itself may be a dotted module
            // path (`src.plain`). Splitting on the FIRST dot mistook the
            // owner's leading path segment for the lexical import binding —
            // `import_spans` keys a selective import by the MODULE's short
            // name (`plain`), not its first path segment (`src`), so that
            // mis-derived key never matched and `Plain { … }` warned
            // "unused import" even though it constructed the imported type.
            // `mark_module_owner_bindings_used` resolves the owner back to
            // the correct lexical binding via `module_import_bindings`,
            // mirroring the working annotation-position credit above.
            if let Some((owner, _)) = qualified.rsplit_once('.') {
                self.mark_module_owner_bindings_used(owner);
            }
        }
        let name = qualified_owned.as_deref().unwrap_or(name);
        if self
            .lookup_type_def(name)
            .is_some_and(|definition| definition.kind == TypeDefKind::Enum)
        {
            self.report_error(
                TypeErrorKind::TypeUsedAsValue,
                span,
                format!("enum `{name}` requires a declared variant; it cannot be constructed as a record"),
            );
            return Ty::Error;
        }
        // Fail closed on opaque handle direct construction — but ONLY for
        // cross-module constructions. The module that DECLARES an `#[opaque]`
        // type is the producer: its impl blocks contain the legitimate FFI
        // constructors (`extern "C"` stubs returning the handle) and must be
        // allowed to write `Handle { }` as the return value stub. Only OTHER
        // modules (importers / users) see it as opaque and must use the
        // declared constructor functions.
        //
        // `local_type_defs` is seeded (in `mod.rs`) with every type NAME
        // declared in the current module before body-checking begins. A bare
        // name that is present there means "this module declared it", so the
        // construction is in-module / producer-side and is ALLOWED.
        //
        // `name` at this point may be qualified (`module.Handle`) after
        // `published_bare_type_qualified` resolves a bare import reference.
        // `user_opaque_type_names` stores exact declaration identities. A
        // same-leaf type from another module must not acquire opacity.
        let unqualified = name.split_once('.').map_or(name, |(_, unqual)| unqual);
        let canonical_owner_is_current_source = name
            .rsplit_once('.')
            .is_some_and(|(owner, _)| self.checking_canonical_stdlib_source(owner));
        let is_declaring_module = self.local_type_defs.contains(unqualified)
            // A bundled stdlib package can contain several source files that
            // are registered into one source-owner frame.  Let that proven
            // source owner build its opaque wrapper stubs, but never extend
            // the exemption to an importer or to a user module with a
            // std-looking spelling.
            || canonical_owner_is_current_source;
        let is_opaque_handle = !is_declaring_module
            && (self.user_opaque_type_names.contains(name)
                || self.canonical_owned_handle_type_name(name).is_some());
        if is_opaque_handle {
            self.report_error(
                TypeErrorKind::OpaqueDirectConstruct {
                    type_name: name.to_string(),
                },
                span,
                format!(
                    "cannot construct opaque type `{name}` directly; \
                     opaque handles are produced by their stdlib constructors \
                     [E_OPAQUE_CONSTRUCT]"
                ),
            );
            return Ty::Error;
        }
        let module_local_name = if is_bare_constructor {
            self.current_module_identity().and_then(|owner| {
                let qualified = format!("{owner}.{unqualified}");
                self.type_def_at(&qualified).map(|_| qualified)
            })
        } else {
            None
        };
        let td = module_local_name
            .as_deref()
            .and_then(|qualified| self.lookup_type_def(qualified))
            .or_else(|| self.lookup_type_def(name));
        if let Some(td) = td {
            // Track inferred type arguments for generic structs.
            // If the caller supplied explicit type args (e.g. `Wrapper<String> { ... }`),
            // pre-seed the map from them so field checking constrains against the
            // declared types immediately rather than synthesizing unconstrained.
            let mut type_arg_map: HashMap<String, Ty> = HashMap::new();
            if let Some(explicit_args) = type_args {
                if explicit_args.len() == td.type_params.len() {
                    for (tp, te) in td.type_params.iter().zip(explicit_args.iter()) {
                        let resolved = self.resolve_type_expr(te);
                        type_arg_map.insert(tp.clone(), resolved);
                    }
                } else {
                    // Covers both `Foo<>` (zero explicit args) and wrong-count args.
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        format!(
                            "{} `{name}` has {} type parameter(s) but {} type argument(s) were supplied",
                            value_type_kind_label(td.kind),
                            td.type_params.len(),
                            explicit_args.len()
                        ),
                    );
                }
            }

            // Pre-seed every still-unbound type parameter with a fresh inference
            // var so a field whose declared type CONTAINS a parameter (e.g.
            // `items: Vec<T>`) constrains that parameter from its initializer,
            // exactly as a field whose type IS the bare parameter (`val: T`)
            // already does. Without this the nested parameter stays the raw
            // type-def symbol `T` in the initializer's own type and never
            // monomorphises (`E_MIR: unknown type T` at the MIR boundary). The
            // vars unify during field checking below and are resolved back to
            // concrete types before the result type is built.
            for tp in &td.type_params {
                type_arg_map
                    .entry(tp.clone())
                    .or_insert_with(|| Ty::Var(TypeVar::fresh()));
            }

            for (field_name, (expr, es)) in fields {
                if let Some(declared_ty) = td.fields.get(field_name.name.as_str()) {
                    // Substitute already-inferred type params into the expected type.
                    // Use parallel substitution so a swap map {"A": B, "B": A} does not
                    // alias both params: each Named leaf is replaced in one structural pass.
                    let expected = declared_ty.substitute_named_params_parallel(&type_arg_map);

                    // If the expected type is still an unbound type parameter,
                    // synthesize so the field value determines the type (rather
                    // than failing with "expected T, found i64").
                    let is_unbound_param = td
                        .type_params
                        .iter()
                        .any(|tp| !type_arg_map.contains_key(tp) && expected == (Ty::param(tp)));
                    let actual = if is_unbound_param {
                        self.synthesize(expr, es)
                    } else {
                        self.check_against(expr, es, &expected)
                    };
                    self.record_value_transfer(expr, es);

                    // Infer type params: if field type is a bare type param, bind it
                    for tp in &td.type_params {
                        if !type_arg_map.contains_key(tp) && *declared_ty == (Ty::param(tp)) {
                            type_arg_map.insert(tp.clone(), actual.clone());
                        }
                    }
                } else {
                    let similar = crate::error::find_similar(
                        field_name.name.as_str(),
                        td.fields.keys().map(String::as_str),
                    );
                    self.report_error_with_suggestions(
                        TypeErrorKind::UndefinedField,
                        span,
                        format!(
                            "no field `{field_name}` on {} `{name}`",
                            value_type_kind_label(td.kind)
                        ),
                        similar,
                    );
                }
            }
            // Functional-update base: `R { x: 5, ..base }`.
            // The base must evaluate to the same named record/struct type.
            // When base is present, fields not listed explicitly are filled from base,
            // so the missing-field check is skipped.
            if let Some((base_expr, base_span)) = base {
                let base_ty = self.synthesize(base_expr, base_span);
                let declared_head = self.named_ty_for_key(name, Vec::new()).head();
                match &base_ty {
                    Ty::Named { head, .. } if Some(*head) == declared_head => {}
                    _ => {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            span,
                            format!(
                                "functional-update base must be of type `{name}`, found `{base_ty}`"
                            ),
                        );
                    }
                }
            } else {
                // No base: all fields must be explicitly provided.
                let provided: HashSet<&str> = fields.iter().map(|(n, _)| n.name.as_str()).collect();
                for declared in td.fields.keys() {
                    if !provided.contains(declared.as_str()) {
                        self.report_error(
                            TypeErrorKind::UndefinedField,
                            span,
                            format!("missing field `{declared}` in initializer of `{name}`"),
                        );
                    }
                }
            }
            // Build type args from inferred bindings
            let type_args: Vec<Ty> = td
                .type_params
                .iter()
                .map(|tp| {
                    type_arg_map.get(tp).map_or_else(
                        || Ty::Var(TypeVar::fresh()),
                        |bound| self.subst.resolve(bound),
                    )
                })
                .collect();
            // Record the resolved type arguments for downstream monomorphisation
            // (HIR registry, MIR per-instantiation RecordLayout).
            //
            // Emit unconditionally: a record-init's type args may only become
            // fully concrete *after* `check_struct_init` returns (e.g. via an
            // outer annotation `let b: Box<int> = Box { value: 1 }`), so
            // eagerly rejecting at emission time would drop entries that the
            // post-inference boundary resolve in `check_program` would have made
            // concrete.  The fail-closed contract (no `Ty::Var` crosses into HIR)
            // is enforced at the output boundary by
            // `validate_record_init_type_args_output_contract` in `admissibility.rs`.
            self.record_concrete_record_init_type_args(span, &type_args);
            // Declaration-bound enforcement on the plain struct-init path.
            // The helper short-circuits cleanly for bound-free names and
            // enforces the TypeDef-owned bound map for every generic nominal
            // whose arguments were inferred from the fields above.
            let result_name = module_local_name.as_deref().unwrap_or(name);
            self.enforce_type_def_instantiation_bounds(result_name, &type_args, span);
            self.named_ty_for_key(result_name, type_args)
        } else if let Some((enum_name, variant_fields, enum_type_params)) =
            self.lookup_struct_variant_init(name)
        {
            // Infer generic type args from field values, mirroring the plain-struct path.
            let mut type_arg_map: HashMap<String, Ty> = HashMap::new();
            // If the caller supplied explicit type args (e.g. `Keeper::Holding<int> { … }`),
            // pre-seed the map so field checking constrains against the declared types
            // rather than synthesizing unconstrained.
            if let Some(explicit_args) = type_args {
                if explicit_args.len() == enum_type_params.len() {
                    for (tp, te) in enum_type_params.iter().zip(explicit_args.iter()) {
                        let resolved = self.resolve_type_expr(te);
                        type_arg_map.insert(tp.clone(), resolved);
                    }
                } else {
                    // Covers both `Variant<>` (zero explicit args) and wrong-count args.
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        format!(
                            "enum variant `{name}` has {} type parameter(s) but {} type argument(s) were supplied",
                            enum_type_params.len(),
                            explicit_args.len()
                        ),
                    );
                }
            }

            for (field_name, (expr, es)) in fields {
                if let Some((_, declared_ty)) = variant_fields
                    .iter()
                    .find(|(n, _)| n == field_name.name.as_str())
                {
                    // Substitute already-inferred type params into the expected type
                    let expected = declared_ty.substitute_named_params_parallel(&type_arg_map);

                    // If the expected type is still an unbound type parameter, synthesize
                    // so the field value determines the concrete type.
                    let is_unbound_param = enum_type_params
                        .iter()
                        .any(|tp| !type_arg_map.contains_key(tp) && expected == (Ty::param(tp)));
                    let actual = if is_unbound_param {
                        self.synthesize(expr, es)
                    } else {
                        self.check_against(expr, es, &expected)
                    };
                    self.record_value_transfer(expr, es);

                    // Bind bare type params from this field's declared type
                    for tp in &enum_type_params {
                        if !type_arg_map.contains_key(tp) && *declared_ty == (Ty::param(tp)) {
                            type_arg_map.insert(tp.clone(), actual.clone());
                        }
                    }
                } else {
                    let similar = crate::error::find_similar(
                        field_name.name.as_str(),
                        variant_fields.iter().map(|(n, _)| n.as_str()),
                    );
                    self.report_error_with_suggestions(
                        TypeErrorKind::UndefinedField,
                        span,
                        format!("no field `{field_name}` on variant `{name}`"),
                        similar,
                    );
                }
            }
            let provided: HashSet<&str> = fields.iter().map(|(n, _)| n.name.as_str()).collect();
            for (declared, _) in &variant_fields {
                if !provided.contains(declared.as_str()) {
                    self.report_error(
                        TypeErrorKind::UndefinedField,
                        span,
                        format!("missing field `{declared}` in initializer of `{name}`"),
                    );
                }
            }
            // Build concrete type args from inferred bindings
            let type_args: Vec<Ty> = enum_type_params
                .iter()
                .map(|tp| {
                    type_arg_map
                        .get(tp)
                        .cloned()
                        .unwrap_or_else(|| Ty::Var(TypeVar::fresh()))
                })
                .collect();
            // Emit unconditionally; see the struct-init branch above for the
            // boundary-prune rationale and validator location.
            self.record_concrete_record_init_type_args(span, &type_args);
            // Enforce trait bounds declared on the enum's generic type
            // parameters via the canonical nominal helper. This keeps
            // struct-variant brace init on the same TypeDef-bound authority as
            // annotations, tuple variants, and plain struct/record init.
            self.enforce_type_def_instantiation_bounds(&enum_name, &type_args, span);
            self.named_ty_for_key(&enum_name, type_args)
        } else {
            let similar = crate::error::find_similar(
                name,
                self.type_defs
                    .keys()
                    .map(|id| self.defs.path(id.declaration()))
                    .chain(self.type_aliases.keys().map(String::as_str))
                    .chain(self.known_types.iter().map(String::as_str)),
            );
            self.report_error_with_suggestions(
                TypeErrorKind::UndefinedType,
                span,
                format!("undefined type `{name}`"),
                similar,
            );
            Ty::Error
        }
    }

    /// Check each constructor argument of a `spawn` expression, pushing the
    /// actor field's declared type down as the expected type so that generic
    /// constructors like `HashMap::new()` and `Vec::new()` can resolve their
    /// type parameters.
    ///
    /// Without this, `spawn Cache(store: HashMap::new())` synthesises the arg
    /// with unbound type variables (`HashMap<?T, ?U>`).  The Send check then
    /// fires on those unbound vars with the misleading message:
    ///   "cannot send `HashMap<?T22, ?T23>` to actor: type is not Send"
    ///
    /// Mirrors `check_struct_init`'s field push-down.  Two exceptions fall back
    /// to `synthesize`:
    ///
    /// 1. Unknown field name — an error will be reported separately.
    /// 2. Bare-actor-name field (e.g. `let target: Printer`): the spawn arg
    ///    carries `Printer`'s own actor-handle type, not the bare `Printer`
    ///    used for construction, so checking against the bare name produces
    ///    a spurious type mismatch.
    pub(super) fn check_spawn_constructor_args(
        &mut self,
        actor_name: &str,
        args: &[(Ident, Spanned<Expr>)],
        type_subst: Option<&HashMap<String, Ty>>,
    ) {
        let actor_fields: Option<HashMap<String, Ty>> =
            self.lookup_type_def(actor_name).map(|td| td.fields);
        // An actor with an explicit `init(...)` names its spawn args after
        // the INIT PARAMETERS, not the state fields they assign into (the
        // two names may differ, and even when they match, the init
        // parameter's declared width is the checker-authoritative one — the
        // init body may narrow/widen before storing into the field). Look up
        // `actor_init_params` first so a param like `init(start: i32)` gets
        // `check_against(..., i32)` here. The field-type lookup is the
        // fallback for two shapes: an actor with no explicit `init` (whose
        // spawn args map directly onto bare field names), and an init-bearing
        // actor whose spawn arg name does not match any init parameter and so
        // routes straight into a same-named state field. Without this, an
        // unmatched `field_name` silently synthesizes the arg (defaulting an
        // untyped int literal to `i64`), which then mismatches the init
        // thunk's declared i32 parameter and trips the LLVM verifier at the
        // spawn call site (#2402).
        let init_params = self.actor_init_params.get(actor_name).cloned();
        for (field_name, (arg, as_)) in args {
            let declared_init_param = init_params
                .as_ref()
                .and_then(|params| params.iter().find(|p| p.name == field_name.name.as_str()))
                .map(|p| p.ty.clone());
            // A field init initializes has no spawn value (D447): one init
            // body cannot be a first store at one spawn site and a
            // replacement at another.
            if declared_init_param.is_none()
                && self
                    .actor_deferred_fields
                    .get(actor_name)
                    .is_some_and(|deferred| {
                        deferred
                            .iter()
                            .any(|field| field == field_name.name.as_str())
                    })
            {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    as_,
                    format!(
                        "E_ACTOR_FIELD_DEFERRED: state field `{field_name}` of actor \
                         `{actor_name}` is initialized by `init`; remove it from the spawn \
                         arguments"
                    ),
                );
                let ty_raw = self.synthesize(arg, as_);
                self.enforce_actor_boundary_send(arg, as_, as_, &ty_raw);
                continue;
            }
            // A spawn arg name that matches BOTH an init parameter and a state
            // field with a DIFFERENT declared type is unsatisfiable: the one
            // provided value cannot simultaneously be the init parameter's type
            // (which the init thunk expects) and the field's type (which the
            // constructor stores it into). Checking the arg against the init
            // parameter alone lets it pass here, but codegen then stores the
            // value into the mismatched field slot and fails closed with a raw
            // RecordInit verifier dump (#2448). Name the collision at the
            // checker level -- the parameter, the field, and the two disagreeing
            // types -- and skip the per-arg check so no confusing secondary
            // diagnostic piles on.
            let field_ty = actor_fields
                .as_ref()
                .and_then(|f| f.get(field_name.name.as_str()));
            if let (Some(param_ty), Some(field_ty)) = (declared_init_param.as_ref(), field_ty) {
                if param_ty != field_ty {
                    let param_display = param_ty.user_facing().to_string();
                    let field_display = field_ty.user_facing().to_string();
                    self.report_error(
                        TypeErrorKind::Mismatch {
                            expected: field_display.clone(),
                            actual: param_display.clone(),
                        },
                        as_,
                        format!(
                            "spawn argument `{field_name}` matches both the `init` \
                             parameter `{field_name}: {param_display}` and the state \
                             field `{field_name}: {field_display}` of actor \
                             `{actor_name}`, whose types disagree; one value cannot \
                             fill both. Rename the `init` parameter or the field so the \
                             spawn argument targets exactly one of them."
                        ),
                    );
                    // Still synthesize the arg so downstream expression typing
                    // sees a type for this span, but do not run `check_against`
                    // (it would emit a second, less-informative mismatch).
                    let ty_raw = self.synthesize(arg, as_);
                    self.enforce_actor_boundary_send(arg, as_, as_, &ty_raw);
                    continue;
                }
            }
            let declared = declared_init_param.as_ref().or_else(|| {
                actor_fields
                    .as_ref()
                    .and_then(|f| f.get(field_name.name.as_str()))
            });
            // Substitute the spawn site's type arguments into the declared
            // type before checking, so a generic field/init param (`value: T`)
            // is compared against the instantiated type (`i64`) rather than
            // the unbound generic `T` (#2447). Non-generic actors and
            // arity-mismatched spawns pass `None` and check against the raw
            // declared type unchanged.
            let declared_owned: Option<Ty> = match (declared, type_subst) {
                (Some(ty), Some(subst)) => Some(ty.substitute_named_params_parallel(subst)),
                _ => None,
            };
            let declared = declared_owned.as_ref().or(declared);
            let ty_raw = match declared {
                Some(declared_ty) => {
                    let is_bare_actor = if let Ty::Named { head, .. } = declared_ty {
                        self.type_def_at(head.registry_key())
                            .is_some_and(|td| td.kind == TypeDefKind::Actor)
                    } else {
                        false
                    };
                    if is_bare_actor {
                        self.synthesize(arg, as_)
                    } else {
                        self.check_against(arg, as_, declared_ty)
                    }
                }
                None => self.synthesize(arg, as_),
            };
            self.enforce_actor_boundary_send(arg, as_, as_, &ty_raw);
        }
    }

    /// Resolve a `spawn` target expression to the registered actor identity.
    ///
    /// `Ok(Some(identity))` carries the identity key (bare for root/flat
    /// actors, dotted `{module}.{name}` for module actors); `Ok(None)` is an
    /// unsupported target shape; `Err(())` means a diagnostic was already
    /// emitted and the spawn must type to bare `Ty::Error`.
    pub(super) fn resolve_spawn_target(
        &mut self,
        target: &Spanned<Expr>,
        span: &Span,
    ) -> Result<Option<String>, ()> {
        Ok(match &target.0 {
            // Bare spawn target: resolve local-first to the registered
            // actor identity (the current module's own actor, then a
            // root/flat actor, then a named-import binding, then a unique
            // module export). A bare name exported by 2+ modules with no
            // local actor is a typed error naming the candidates — never
            // silent first-wins.
            Expr::Ident(name) => {
                match self.resolve_bare_spawn_target_identity(name.name.as_str()) {
                    super::types::BareActorResolution::Resolved(identity) => Some(identity),
                    super::types::BareActorResolution::Ambiguous(candidate_modules) => {
                        self.report_ambiguous_actor_reference(
                            name.name.as_str(),
                            &candidate_modules,
                            span,
                        );
                        return Err(());
                    }
                    // Unknown actor: keep the bare name so the pre-existing
                    // unknown-actor diagnostics downstream fire unchanged.
                    super::types::BareActorResolution::Unknown => Some(name.to_string()),
                }
            }
            // Handle module-qualified actor: spawn module.ActorName(args)
            Expr::FieldAccess { object, field } => {
                if let Expr::Ident(module) = &object.0 {
                    if self.module_binding_in_current_file(module.name.as_str()) {
                        // Verify the qualifier resolves to something spawnable
                        // that is a public export of `module` before stripping
                        // it to the bare name. `module_type_exports` membership
                        // alone is insufficient: that set also holds public
                        // non-spawnable types, so it is true even when
                        // `secret.Account` is a `pub type`/struct/enum (and a
                        // private actor is absent from it entirely). Resolve the
                        // qualified definition and require an actor or a
                        // supervisor, which spawn the same way; otherwise
                        // `spawn secret.Account()` would lower to bare `Account`
                        // and silently route to a same-named root/pub actor -- a
                        // capability-boundary hole. `resolve_module_type` already
                        // gates on `pub` export + the module-qualified `type_defs`
                        // entry (which is copied from the module's own decl, so it
                        // is not clobbered by a same-named root/other-module type).
                        // Fail closed before HIR/MIR rather than misroute.
                        let actor_identity = self
                            .resolve_module_type(module.name.as_str(), field.0.name.as_str())
                            .filter(|td| {
                                matches!(td.kind, TypeDefKind::Actor | TypeDefKind::Supervisor)
                            })
                            .map(|td| td.name);
                        let Some(actor_identity) = actor_identity else {
                            let similar = self
                                .module_type_exports_for_binding(module.name.as_str())
                                .map(|set| {
                                    crate::error::find_similar(
                                        field.0.name.as_str(),
                                        set.iter().map(String::as_str),
                                    )
                                })
                                .unwrap_or_default();
                            self.report_error_with_suggestions(
                                TypeErrorKind::UndefinedType,
                                span,
                                format!(
                                    "module `{module}` has no exported actor or supervisor \
                                     `{}`",
                                    field.0
                                ),
                                similar,
                            );
                            // The caller types the spawn as bare `Ty::Error`
                            // (not `Error`'s own actor-handle type) so a subsequent
                            // `await handle.method()` is suppressed (method
                            // calls on a `Ty::Error` receiver short-circuit),
                            // keeping a single clear diagnostic.
                            return Err(());
                        };
                        self.used_modules.borrow_mut().insert(ImportKey::in_file(
                            self.current_module.clone(),
                            self.current_module_idx,
                            module.to_string(),
                        ));
                        // Keep the exact source identity recovered through the
                        // lexical module binding. The surface spelling may be
                        // an alias or share its leaf with another module.
                        Some(actor_identity)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        })
    }

    pub(in crate::check) fn check_spawn(
        &mut self,
        target: &Spanned<Expr>,
        type_args: &[Spanned<TypeExpr>],
        args: &[(Ident, Spanned<Expr>)],
        span: &Span,
    ) -> Ty {
        let Ok(actor_name) = self.resolve_spawn_target(target, span) else {
            return Ty::Error;
        };

        if let Some(name) = actor_name {
            let owner_kind = if self.supervisor_children.contains_key(&name) {
                "supervisor"
            } else {
                "actor"
            };
            let type_params = self
                .type_def_at(&name)
                .map_or_else(Vec::new, |definition| definition.type_params.clone());
            let declared_arity = type_params.len();
            let mut resolved_type_args: Vec<Ty> = if type_args.is_empty() {
                type_params
                    .iter()
                    .map(|_| Ty::Var(TypeVar::fresh()))
                    .collect()
            } else {
                type_args
                    .iter()
                    .map(|argument| self.resolve_type_expr(argument))
                    .collect()
            };
            if resolved_type_args.len() != declared_arity {
                self.report_error(
                    TypeErrorKind::ActorTypeArgArityMismatch {
                        actor_name: name.clone(), expected: declared_arity, got: resolved_type_args.len(),
                    }, span,
                    format!("{owner_kind} `{name}` has {declared_arity} type parameter(s) but {} type argument(s) were supplied", resolved_type_args.len()),
                );
                return Ty::Error;
            }
            let type_subst: HashMap<_, _> = type_params
                .iter()
                .cloned()
                .zip(resolved_type_args.iter().cloned())
                .collect();
            self.check_spawn_constructor_args(&name, args, Some(&type_subst));
            if let Some(expected_args) = self.actor_spawn_args.get(&name).cloned() {
                for (argument, required) in expected_args {
                    if required
                        && !args
                            .iter()
                            .any(|(provided, _)| provided.name.as_str() == argument)
                    {
                        self.report_error(
                            TypeErrorKind::MissingActorSpawnArgument,
                            span,
                            format!(
                                "actor `{name}` requires an initialized spawn value for `{argument}`"
                            ),
                        );
                    }
                }
            }
            resolved_type_args = resolved_type_args
                .iter()
                .map(|argument| self.subst.resolve(argument))
                .collect();
            if resolved_type_args.iter().any(Ty::has_inference_var) {
                self.report_error(
                    TypeErrorKind::MissingActorTypeArgs { actor_name: name.clone(), expected_arity: declared_arity },
                    span,
                    format!("cannot infer all type arguments of {owner_kind} `{name}` from its spawn arguments; supply explicit type arguments"),
                );
                return Ty::Error;
            }
            self.enforce_type_def_instantiation_bounds(&name, &resolved_type_args, span);

            match self.nominal_head_for_key(&name) {
                Some(actor) => Ty::actor_handle(actor, resolved_type_args),
                None => Ty::Error,
            }
        } else {
            Ty::Error
        }
    }

    /// Report the typed ambiguity error for a bare actor reference that is
    /// exported by two or more modules with no local actor to win the
    /// local-first resolution. Names every candidate and suggests the
    /// qualified spawn spelling — never silent first-wins.
    pub(super) fn report_ambiguous_actor_reference(
        &mut self,
        name: &str,
        candidate_modules: &[String],
        span: &Span,
    ) {
        let candidate_identities: Vec<String> = candidate_modules
            .iter()
            .map(|module| format!("{module}.{name}"))
            .collect();
        self.mark_ambiguous_import_owners_used(&candidate_identities);
        let candidates_list = candidate_modules
            .iter()
            .map(|m| format!("`{m}.{name}`"))
            .collect::<Vec<_>>()
            .join(", ");
        let qualified_examples = candidate_modules
            .iter()
            .map(|m| format!("`spawn {m}.{name}(...)`"))
            .collect::<Vec<_>>()
            .join(" or ");
        self.report_error_with_suggestions(
            TypeErrorKind::AmbiguousActorReference {
                actor_name: name.to_string(),
                candidate_modules: candidate_modules.to_vec(),
            },
            span,
            format!(
                "actor `{name}` is ambiguous: it is exported by multiple \
                 modules ({candidates_list}) and no local actor `{name}` \
                 exists to take precedence"
            ),
            vec![format!(
                "qualify the spawn target with its module: {qualified_examples}"
            )],
        );
    }
}
