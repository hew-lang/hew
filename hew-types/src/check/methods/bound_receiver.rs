use super::{
    CallArg, CallTarget, Checker, Expr, FnSig, MethodCallReceiverKind, MethodCallRewrite, Span,
    SpanKey, Spanned, Ty, TypeErrorKind,
};

impl Checker {
    pub(in crate::check) fn check_named_index(
        &mut self,
        receiver: &Ty,
        index: &Spanned<Expr>,
        trait_id: crate::DefId,
        span: &Span,
    ) -> Option<Ty> {
        let (head, instance) = self.dispatch_receiver(receiver)?;
        let declaration = match self.dispatch.select_trait(
            head,
            instance.as_ref(),
            trait_id,
            crate::Symbol::intern("at"),
        ) {
            super::super::dispatch_table::MethodSelection::Unique(_, declaration) => declaration,
            super::super::dispatch_table::MethodSelection::Ambiguous(traits) => {
                self.synthesize(&index.0, &index.1);
                self.report_ambiguous_method(receiver, "at", &traits, span);
                return Some(Ty::Error);
            }
            super::super::dispatch_table::MethodSelection::Missing => return None,
        };
        let signature = self.selected_method_sig(receiver, declaration)?;
        let Ty::Named { head, args } = receiver else {
            return None;
        };
        let applied = self.apply_instantiated_call_signature_with_assoc(
            &signature,
            None,
            &[CallArg::Positional(index.clone())],
            span,
            super::super::calls::SignatureArgApplication::PositionalOnly {
                arity_context: "Index.at".into(),
            },
            true,
            Some(super::super::types::GenericCallee::Method {
                type_name: head.registry_key(),
                method: "at",
                owner_type_args: args,
            }),
        );
        self.record_method_call_receiver_kind(
            span,
            MethodCallReceiverKind::NamedTypeInstance {
                type_name: head.registry_key().to_string(),
            },
        );
        self.record_selected_method_rewrite(receiver, "at", declaration, &signature, span);
        Some(self.project_assoc_types(&applied.return_type))
    }

    pub(in crate::check) fn check_bound_receiver_method(
        &mut self,
        receiver: &Spanned<Expr>,
        receiver_ty: &Ty,
        method: &str,
        args: &[CallArg],
        span: &Span,
        required_trait: Option<crate::DefId>,
    ) -> Option<Ty> {
        let normalized = self.normalize_for_use(receiver_ty);
        let resolved = self
            .bound_parameter(&normalized)
            .map_or(normalized, Ty::param);
        let Ty::Named {
            head: crate::TypeHead::Param(parameter),
            ..
        } = &resolved
        else {
            return None;
        };
        let name = parameter.spelling.as_str();
        let (bounds, hits) =
            self.bound_receiver_method_candidates(*parameter, receiver_ty, method, required_trait);
        if hits.len() == 1 {
            let (declaring_id, trait_sig) = hits.into_iter().next().unwrap();
            let declaring_trait = self.defs.path(declaring_id).to_string();
            let self_ty = self.normalize_for_use(receiver_ty);
            if self.refuse_associated_dot_call(&trait_sig, &declaring_trait, method, args, span) {
                return Some(Ty::Error);
            }
            if trait_sig.requires_mutable_receiver {
                self.check_mutable_method_receiver(
                    receiver,
                    &format!("trait method `{declaring_trait}.{method}` (statically dispatched on type parameter `{name}`)"),
                    span,
                );
            }
            let selected_bounds: Vec<_> = bounds
                .iter()
                .filter(|bound| bound.trait_id == declaring_id)
                .cloned()
                .collect();
            let Some((applied_sig, selected)) = self.apply_call_over_bounds(
                self_ty,
                (name, "bounds"),
                (declaring_id, trait_sig.clone(), &selected_bounds),
                (method, &declaring_trait),
                args,
                span,
            ) else {
                return Some(Ty::Error);
            };
            let trait_args = selected_bounds[selected]
                .args
                .iter()
                .map(|ty| self.subst.resolve(ty))
                .collect();
            if declaring_trait == "std.builtins.Pid" && method == "send" {
                // TODO(A640): replace this fail-closed branch with
                // a first-class `P::Msg: Serializable` projection
                // bound once the checker can express that shape on
                // pid-polymorphic call sites. If the projection is
                // already concretely bound (for example
                // `P: Pid<Msg = Ping>`), the regular Serializable
                // gate below proves it and the call may proceed.
                if !self.enforce_pid_polymorphic_send_serializable_args(args, name) {
                    return Some(Ty::Error);
                }
                self.enforce_actor_method_send_args(args);
            }
            self.record_method_call_receiver_kind(
                span,
                MethodCallReceiverKind::NamedTypeInstance {
                    type_name: name.to_string(),
                },
            );
            if trait_sig.consumes_receiver {
                self.method_call_consumes_receiver
                    .insert(SpanKey::in_module(span, self.current_module_idx));
                let resolved_ty = self.subst.resolve(receiver_ty);
                self.mark_expr_moved_if_non_copy(&receiver.0, &receiver.1, &resolved_ty);
            }
            self.record_bound_method_rewrite(
                span,
                *parameter,
                declaring_id,
                method,
                trait_args,
                &trait_sig,
            );
            return Some(self.project_assoc_types(&applied_sig.return_type));
        } else if hits.len() > 1 {
            for arg in args {
                let (expr, sp) = arg.expr();
                self.synthesize(expr, sp);
            }
            let mut declaring_traits: Vec<&str> =
                hits.iter().map(|hit| self.defs.path(hit.0)).collect();
            declaring_traits.sort_unstable();
            self.report_error(
                TypeErrorKind::AmbiguousTraitMethod,
                span,
                format!(
                    "ambiguous trait method `{method}` on `{}`: method is declared by \
                     multiple traits ({}); qualify the call to disambiguate",
                    resolved.user_facing(),
                    declaring_traits.join(", ")
                ),
            );
            return Some(Ty::Error);
        }
        None
    }
    fn bound_receiver_method_candidates(
        &mut self,
        parameter: crate::ParamHead,
        _receiver_ty: &Ty,
        method: &str,
        required_trait: Option<crate::DefId>,
    ) -> (Vec<super::TraitRef>, Vec<(crate::DefId, FnSig)>) {
        let bounds: Vec<_> = self
            .active_bounds_of(parameter.id)
            .into_iter()
            .filter(|bound| required_trait.is_none_or(|required| bound.trait_id == required))
            .collect();
        let mut hits = Vec::new();
        for bound in &bounds {
            for declaring in self.collect_all_declaring_traits_for_method(bound.trait_id, method) {
                if let Some((_, signature)) =
                    self.lookup_trait_method_with_origin(declaring, method)
                {
                    hits.push((declaring, signature));
                }
            }
        }
        hits.sort_by_key(|hit| hit.0);
        hits.dedup_by_key(|hit| hit.0);
        (bounds, hits)
    }

    fn record_bound_method_rewrite(
        &mut self,
        span: &Span,
        receiver_parameter: crate::ParamHead,
        declaring_id: crate::DefId,
        method: &str,
        trait_args: Vec<Ty>,
        signature: &FnSig,
    ) {
        let declaring_trait = self.defs.path(declaring_id);
        let target = self
            .trait_method_ids_of(declaring_id, method)
            .map_or_else(
                || CallTarget::Unsupported {
                    reason: format!(
                        "trait method `{declaring_trait}.{method}` has no registered declaration identity"
                    ),
                },
                |(declaring_trait, method)| CallTarget::StaticTraitMethod {
                    declaring_trait,
                    method,
                },
            );
        self.record_method_call_rewrite(
            span,
            MethodCallRewrite::StaticTraitDispatch {
                target,
                receiver_type_param: receiver_parameter,
                trait_args,
                requires_mutable_receiver: signature.requires_mutable_receiver,
                consumes_receiver: signature.consumes_receiver,
                returns_receiver_identity: signature.returns_receiver_identity,
            },
        );
    }
}
