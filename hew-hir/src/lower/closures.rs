//! Closure and generator-block lowering.

use super::*;

impl LowerCtx {
    pub(super) fn checked_actor_lambda_handle_ty(&mut self, span: &Span) -> ResolvedTy {
        if let Some(ty) = self.checker_expr_ty_if_present(span) {
            if ty.is_builtin(hew_types::BuiltinType::ActorFn)
                && matches!(&ty, ResolvedTy::Named { args, .. } if args.len() == 2)
            {
                return ty;
            }
        }
        self.diagnostics.push(HirDiagnostic::new(
            HirDiagnosticKind::CheckerBoundaryViolation {
                name: "actor lambda".into(),
                reason: "missing checked actor message and reply types".into(),
            },
            span.clone(),
            "actor lambda lowering requires its checked handle type",
        ));
        ResolvedTy::Unit
    }

    pub(super) fn closure_signature_from_ty(
        ty: &ResolvedTy,
    ) -> Option<(Vec<hew_types::CallableParameter<ResolvedTy>>, ResolvedTy)> {
        match ty.callable_body() {
            ResolvedTy::Function { params, ret, .. } | ResolvedTy::Closure { params, ret, .. } => {
                Some((params.clone(), ret.as_ref().clone()))
            }
            _ => None,
        }
    }

    pub(super) fn visible_outer_bindings(&self) -> HashMap<BindingId, OuterClosureBinding> {
        self.binding_scopes
            .iter()
            .flat_map(|scope| scope.iter())
            .map(|(id, binding)| (*id, binding.clone()))
            .collect()
    }

    /// Materialize the checker's exact generator capture set through checked
    /// binding identities. Send and construction consume the same facts.
    pub(super) fn collect_gen_captures(
        &mut self,
        producer: &hew_types::ty::EffectBody,
        outer_bindings: &HashMap<BindingId, OuterClosureBinding>,
        span: &Span,
    ) -> Vec<HirGenCapture> {
        let Some(facts) = self.generator_capture_facts.get(producer).cloned() else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "generator".into(),
                    reason: "missing checked capture set".into(),
                },
                span.clone(),
                "generator construction requires checked captures",
            ));
            return Vec::new();
        };
        let mut captures = Vec::with_capacity(facts.len());
        for fact in facts {
            let binding = self.lookup_checked(fact.binding_id).and_then(|(id, ty)| {
                outer_bindings
                    .get(&id)
                    .map(|(_, _, _, mutable)| (id, ty, *mutable))
            });
            if let Some((binding, ty, mutable)) = binding {
                captures.push(HirGenCapture {
                    binding,
                    name: fact.name,
                    ty,
                    mutable,
                });
            } else {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: fact.name,
                        reason: "checked capture has no enclosing HIR binding".into(),
                    },
                    span.clone(),
                    "generator capture lost its checked binding identity",
                ));
            }
        }
        captures
    }

    pub(super) fn lower_gen_block(
        &mut self,
        body: &Block,
        span: std::ops::Range<usize>,
    ) -> (HirExprKind, ResolvedTy) {
        let checker_key = self.mk_key(&span);
        let gen_ty = if let Some(ty) = self.expr_types.get(&checker_key).cloned() {
            match ResolvedTy::from_ty(&ty) {
                Ok(resolved) => resolved,
                Err(err) => {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: "gen block".to_string(),
                            reason: err.to_string(),
                        },
                        span.clone(),
                        "gen block type failed checker-boundary conversion",
                    ));
                    ResolvedTy::Named {
                        args: vec![ResolvedTy::Unit, ResolvedTy::Unit],
                        head: hew_types::TypeHead::Builtin(hew_types::BuiltinType::Generator),
                        is_opaque: false,
                    }
                }
            }
        } else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "gen block".to_string(),
                    reason: "expr_types has no entry for gen block site".to_string(),
                },
                span.clone(),
                "checker did not provide a Generator<Yield, Return> type for this gen block",
            ));
            ResolvedTy::Named {
                args: vec![ResolvedTy::Unit, ResolvedTy::Unit],
                head: hew_types::TypeHead::Builtin(hew_types::BuiltinType::Generator),
                is_opaque: false,
            }
        };

        let (yield_ty, return_ty) =
            Self::generator_yield_return_parts(&gen_ty).unwrap_or_else(|| {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: "gen block".to_string(),
                        reason: format!("expected Generator<Yield, Return>, got {gen_ty:?}"),
                    },
                    span.clone(),
                    "gen block checker type did not have Generator<Yield, Return> shape",
                ));
                (ResolvedTy::Unit, ResolvedTy::Unit)
            });

        // Snapshot the enclosing scope BEFORE lowering the body so the
        // gen-block's own `let`/`var` bindings are excluded and only captured
        // outer locals are candidates. See `collect_gen_captures`.
        let outer_bindings = self.visible_outer_bindings();
        self.generator_yield_tys.push(yield_ty.clone());
        let enclosing_failing = self.failing_generator_item.take();
        let lowered_body = self
            .with_current_return_type(return_ty.clone(), |ctx| ctx.lower_block(body, &return_ty));
        self.failing_generator_item = enclosing_failing;
        self.generator_yield_tys.pop();
        let captures = self.collect_gen_captures(
            &hew_types::ty::EffectBody::GeneratorBlock(checker_key),
            &outer_bindings,
            &span,
        );

        (
            HirExprKind::GenBlock {
                body: lowered_body,
                yield_ty,
                return_ty,
                captures,
            },
            gen_ty,
        )
    }

    pub(super) fn generator_yield_return_parts(
        ty: &ResolvedTy,
    ) -> Option<(ResolvedTy, ResolvedTy)> {
        ty.generator_parts()
            .map(|(yields, returns)| (yields.clone(), returns.clone()))
    }

    pub(super) fn reject_closure_boundary(
        &mut self,
        span: std::ops::Range<usize>,
        reason: impl Into<String>,
    ) -> (HirExprKind, ResolvedTy) {
        self.diagnostics.push(HirDiagnostic::new(
            HirDiagnosticKind::CheckerBoundaryViolation {
                name: "closure literal".to_string(),
                reason: reason.into(),
            },
            span,
            "closure literal reached HIR without valid checker-owned facts",
        ));
        (HirExprKind::Literal(HirLiteral::Unit), ResolvedTy::Unit)
    }

    pub(super) fn lower_closure(
        &mut self,
        params: &[LambdaParam],
        body: &Spanned<Expr>,
        span: std::ops::Range<usize>,
    ) -> (HirExprKind, ResolvedTy) {
        let checker_key = self.mk_key(&span);
        let Some(ty) = self.expr_types.get(&checker_key) else {
            return self.reject_closure_boundary(
                span,
                "expr_types has no callable type for closure literal span",
            );
        };
        let closure_ty = match ResolvedTy::from_ty(ty) {
            Ok(ty) => ty,
            Err(error) => return self.reject_closure_boundary(span, error.to_string()),
        };
        let Some((signature_params, ret_ty)) = Self::closure_signature_from_ty(&closure_ty) else {
            return self.reject_closure_boundary(
                span,
                format!("expected Function/Closure type, got {closure_ty:?}"),
            );
        };
        if signature_params.len() != params.len() {
            return self.reject_closure_boundary(
                span,
                "checker callable signature does not match closure parameter count",
            );
        }
        let Some(checker_facts) = self.closure_capture_facts.get(&checker_key).cloned() else {
            return self.reject_closure_boundary(
                span,
                "closure_capture_facts has no record for closure literal span",
            );
        };
        let Some(escape_kind) = self
            .closure_escape_facts
            .get(&checker_key)
            .map(|fact| fact.kind)
        else {
            return self.reject_closure_boundary(
                span,
                "closure_escape_facts has no record for closure literal span",
            );
        };
        let outer_bindings = self.visible_outer_bindings();
        self.push_scope();
        let hir_params = params
            .iter()
            .zip(signature_params)
            .map(|(param, signature)| {
                let mut binding = self.bind_checked(
                    param.name.to_string(),
                    signature.ty,
                    false,
                    param.name_span.clone(),
                );
                binding.is_consume = signature.ownership == hew_types::ParameterOwnership::Consume;
                binding
            })
            .collect();
        let enclosing_failing = self.failing_generator_item.take();
        let lowered_body = self
            .with_current_return_type(ret_ty.clone(), |ctx| ctx.lower_expr(body, IntentKind::Read));
        self.failing_generator_item = enclosing_failing;
        self.pop_scope();
        let captures =
            self.materialize_closure_captures(&lowered_body, &outer_bindings, checker_facts, span);
        (
            HirExprKind::Closure {
                params: hir_params,
                ret_ty,
                body: Box::new(lowered_body),
                captures,
                escape_kind,
            },
            closure_ty,
        )
    }

    pub(super) fn match_payload_binding_span(
        &mut self,
        payload: &hew_types::PayloadBinding,
        pattern_span: &Span,
    ) -> Option<Span> {
        if let Some(span) = &payload.def_span {
            return Some(span.clone());
        }
        self.diagnostics.push(HirDiagnostic::new(
            HirDiagnosticKind::CheckerBoundaryViolation {
                name: payload.binding_name.clone(),
                reason: "match payload binding has no checker definition occurrence".into(),
            },
            pattern_span.clone(),
            "checker did not retain the match payload binding's definition occurrence",
        ));
        None
    }

    pub(super) fn materialize_closure_captures(
        &mut self,
        body: &HirExpr,
        outer_bindings: &HashMap<BindingId, OuterClosureBinding>,
        facts: Vec<ClosureCaptureFact>,
        span: std::ops::Range<usize>,
    ) -> Vec<HirClosureCapture> {
        let mut seen: HashSet<BindingId> = HashSet::new();
        let mut ordered: Vec<ClosureCaptureCandidate> = Vec::new();
        collect_general_closure_captures_walk(body, outer_bindings, &mut seen, &mut ordered);

        self.materialize_closure_capture_candidates(ordered, facts, span)
    }

    pub(super) fn materialize_closure_block_captures(
        &mut self,
        body: &HirBlock,
        outer_bindings: &HashMap<BindingId, OuterClosureBinding>,
        facts: Vec<ClosureCaptureFact>,
        span: std::ops::Range<usize>,
    ) -> Vec<HirClosureCapture> {
        let mut seen: HashSet<BindingId> = HashSet::new();
        let mut ordered: Vec<ClosureCaptureCandidate> = Vec::new();
        collect_general_closure_captures_walk_block(body, outer_bindings, &mut seen, &mut ordered);
        self.materialize_closure_capture_candidates(ordered, facts, span)
    }

    pub(super) fn materialize_closure_capture_candidates(
        &mut self,
        ordered: Vec<ClosureCaptureCandidate>,
        facts: Vec<ClosureCaptureFact>,
        span: std::ops::Range<usize>,
    ) -> Vec<HirClosureCapture> {
        let mut remaining_facts = facts;
        let mut captures = Vec::with_capacity(ordered.len());
        for (binding, name, def_span) in ordered {
            let fact_idx = remaining_facts.iter().position(|fact| {
                fact.name == name
                    && fact
                        .def_span
                        .as_ref()
                        .is_some_and(|fact_def_span| *fact_def_span == def_span)
            });
            let Some(fact_idx) = fact_idx else {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: name.clone(),
                        reason:
                            "closure_capture_facts has no unambiguous entry for captured HIR binding"
                                .to_string(),
                    },
                    span.clone(),
                    "closure capture reached HIR without checker materialization metadata",
                ));
                continue;
            };
            let fact = remaining_facts.remove(fact_idx);
            let ty = match ResolvedTy::from_ty(&fact.ty) {
                Ok(ty) => self.restore_type_declaration_facts(ty),
                Err(err) => {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: fact.name.clone(),
                            reason: err.to_string(),
                        },
                        span.clone(),
                        "closure capture type failed checker-boundary conversion",
                    ));
                    continue;
                }
            };
            captures.push(HirClosureCapture {
                binding,
                name,
                ty,
                acquisition: fact.acquisition,
                access: fact.access,
                consumption: fact.consumption,
                is_send: fact.is_send,
                is_sync: fact.is_sync,
            });
        }

        for fact in remaining_facts {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: fact.name,
                    reason:
                        "checker reported a capture that HIR name resolution did not materialize"
                            .to_string(),
                },
                span.clone(),
                "closure capture metadata and lowered HIR body disagree",
            ));
        }

        captures
    }
}
