//! Actor ask results and `select` lowering.

use super::*;

impl LowerCtx {
    /// Preserve the checked call result separately from the raw actor reply ABI.
    /// The addressed description a submission consumes, read out of the
    /// checked `Result<Delivery, SendFailure<M>>` the call site carries.
    pub(super) fn submitted_message_ty(ty: &ResolvedTy) -> Option<ResolvedTy> {
        let ResolvedTy::Named {
            builtin: Some(BuiltinType::Result),
            args,
            ..
        } = ty
        else {
            return None;
        };
        let [_, ResolvedTy::Named { name, args, .. }] = args.as_slice() else {
            return None;
        };
        if name != hew_types::actor_delivery::FAILURE_TYPE {
            return None;
        }
        let [message] = args.as_slice() else {
            return None;
        };
        Some(message.clone())
    }

    pub(super) fn checked_actor_ask_result_ty(
        &mut self,
        span: &Span,
        method_id: &str,
    ) -> Option<ResolvedTy> {
        let result = self
            .expr_types
            .get(&self.mk_key(span))
            .ok_or_else(|| "missing checker expression type".to_string())
            .and_then(|ty| ResolvedTy::from_ty(ty).map_err(|err| err.to_string()));
        let result_ty = match result {
            Ok(
                ty @ ResolvedTy::Named {
                    builtin: Some(BuiltinType::Result),
                    ..
                },
            ) => ty,
            Ok(ty) => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: "actor ask result".to_string(),
                        reason: format!("expected checked Result, found {ty}"),
                    },
                    span.clone(),
                    "actor ask calls must preserve their checked error result",
                ));
                return None;
            }
            Err(reason) => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: "actor ask result".to_string(),
                        reason,
                    },
                    span.clone(),
                    "actor ask result must cross the checker/HIR boundary exactly",
                ));
                return None;
            }
        };
        let result_ty = Self::actor_module_short_of_method_id(method_id).map_or_else(
            || result_ty.clone(),
            |module| self.qualify_colliding_module_record_ty(&result_ty, module),
        );
        self.try_register_enum_instantiation_ty(&result_ty, span);
        Some(result_ty)
    }

    /// Derive the HIR binding type for a select arm's named pattern.
    ///
    /// For `ActorAsk` arms the full result comes from the checked call type,
    /// keyed on the arm source expression's span.
    /// The source expression's checker-resolved builtin discriminator is the
    /// sole authority for channel and stream carriers. A malformed carrier
    /// returns `None` after recording a boundary diagnostic; it must never be
    /// represented as `Unit`, because MIR would otherwise treat that placeholder
    /// as a real runtime layout witness.
    #[expect(
        clippy::single_match_else,
        reason = "each sealed select carrier has a distinct exact-type diagnostic"
    )]
    pub(super) fn select_arm_binding_ty(
        &mut self,
        kind: &HirSelectArmKind,
        source_span: &std::ops::Range<usize>,
    ) -> Option<ResolvedTy> {
        match kind {
            HirSelectArmKind::ActorAsk { call } => Some(call.ty.clone()),
            // A stream arm wins with the receive result: `None` is EOF.
            HirSelectArmKind::StreamNext { stream } => match &stream.ty {
                ResolvedTy::Named {
                    args,
                    builtin: Some(BuiltinType::Stream),
                    ..
                } if args.len() == 1 => Some(LowerCtx::resolved_option_ty(args[0].clone())),
                _ => {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: "select stream next".to_string(),
                            reason: format!("expected builtin Stream<T>, found {}", stream.ty),
                        },
                        source_span.clone(),
                        "stream-next binding types require the builtin Stream discriminator",
                    ));
                    None
                }
            },
            HirSelectArmKind::TaskAwait { task } => match &task.ty {
                ResolvedTy::Task(inner) => Some((**inner).clone()),
                _ => {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: "select task await".to_string(),
                            reason: format!("expected Task<T>, found {}", task.ty),
                        },
                        source_span.clone(),
                        "task-await binding types require a resolved Task<T>",
                    ));
                    None
                }
            },
            HirSelectArmKind::AfterTimer { .. } => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: "select timer binding".to_string(),
                        reason: "after arms do not produce a value".to_string(),
                    },
                    source_span.clone(),
                    "an `after` arm cannot bind a value",
                ));
                None
            }
        }
    }

    /// Lower selection using the checker's source-arm classifications.
    /// Preparation preserves task handles; the selected edge consumes its task.
    #[allow(
        clippy::too_many_lines,
        reason = "sealed select lowering keeps arm scope publication, binding, and result-type checks in one auditable pass"
    )]
    pub(super) fn lower_select(
        &mut self,
        arms: &[SelectArm],
        timeout: Option<&TimeoutClause>,
        span: std::ops::Range<usize>,
    ) -> (HirExprKind, ResolvedTy) {
        // Multiple `after` arms have no meaningful join semantics and are
        // rejected. An `Expr::Timeout`-sourced arm in the `arms` vec is
        // treated as an `AfterTimer` arm; if that is combined with the
        // dedicated `timeout` field (or if two appear in `arms`) the
        // second one triggers `SelectMultipleAfterArms`.

        let mut hir_arms: Vec<HirSelectArm> = Vec::with_capacity(arms.len() + 1);
        let Some(result_ty) = self.checker_expr_ty(&span, "select result") else {
            return (
                HirExprKind::Unsupported("untyped select".into()),
                ResolvedTy::Unit,
            );
        };
        let mut first_after_span: Option<std::ops::Range<usize>> = None;

        let checked_sources = self.select_sources.get(&self.mk_key(&span)).cloned();
        for (arm_index, arm) in arms.iter().enumerate() {
            let binding_name = self.pattern_name(&arm.binding);
            let checked_source = checked_sources
                .as_ref()
                .and_then(|sources| sources.get(arm_index));
            let kind = self.lower_checked_select_source(&arm.source, checked_source);
            let binding_span = match &arm.source.0 {
                Expr::Await(inner) => &inner.1,
                _ => &arm.source.1,
            };
            if matches!(kind, HirSelectArmKind::AfterTimer { .. }) {
                if first_after_span.is_some() {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::SelectMultipleAfterArms,
                        arm.source.1.clone(),
                        "select may have at most one `after` arm",
                    ));
                } else {
                    first_after_span = Some(arm.source.1.clone());
                }
            }
            // Each arm body lowers inside its own scope so `binding_name`
            // resolves to this arm's reply value within the body, and is
            // invisible to sibling arms and the surrounding scope.
            //
            // The `BindingId` returned by `bind()` is recorded on the
            // arm so MIR's `Terminator::Select` producer can register
            // `binding_locals[id] = <reply_dest_place>` before lowering
            // the arm body; without this id the body's `BindingRef`
            // resolves to an unbound `Place` and falls back to
            // `MirDiagnosticKind::UnresolvedPlace`.
            self.push_scope();
            let arm_scope = binding_name.as_ref().map(|_| self.ids.scope());
            let previous_scope_id =
                arm_scope.map(|scope| std::mem::replace(&mut self.current_scope_id, scope));
            let binding_id = if let Some(ref name) = binding_name {
                self.select_arm_binding_ty(&kind, binding_span)
                    .map(|binding_ty| {
                        self.bind(name.clone(), binding_ty, false, arm.binding.1.clone())
                            .id
                    })
            } else {
                None
            };
            let body = self.lower_expr(&arm.body, IntentKind::Read);
            if let Some(previous) = previous_scope_id {
                self.current_scope_id = previous;
            }
            self.pop_scope();
            if body.ty != result_ty && body.ty != ResolvedTy::Never {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::SelectArmTypeMismatch {
                        arm_index: hir_arms.len(),
                        expected: result_ty.clone(),
                        actual: body.ty.clone(),
                    },
                    arm.body.1.clone(),
                    "select arm body type differs from the first arm body type",
                ));
            }
            hir_arms.push(HirSelectArm {
                scope: arm_scope,
                kind,
                binding_name,
                binding_id,
                body,
            });
        }

        if let Some(timeout) = timeout {
            if first_after_span.is_some() {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::SelectMultipleAfterArms,
                    timeout.duration.1.clone(),
                    "select may have at most one `after` arm",
                ));
            }
            let duration = self.lower_expr(&timeout.duration, IntentKind::Read);
            let body = self.lower_expr(&timeout.body, IntentKind::Read);
            if body.ty != result_ty && body.ty != ResolvedTy::Never {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::SelectArmTypeMismatch {
                        arm_index: hir_arms.len(),
                        expected: result_ty.clone(),
                        actual: body.ty.clone(),
                    },
                    timeout.body.1.clone(),
                    "select after-arm body type differs from earlier arm body types",
                ));
            }
            hir_arms.push(HirSelectArm {
                scope: None,
                kind: HirSelectArmKind::AfterTimer {
                    duration: Box::new(duration),
                },
                binding_name: None,
                binding_id: None,
                body,
            });
        }

        (
            HirExprKind::Select(HirSelect {
                order: crate::HirSelectionOrder::Source,
                arms: hir_arms,
            }),
            result_ty,
        )
    }

    pub(super) fn lower_checked_select_source(
        &mut self,
        source: &Spanned<Expr>,
        checked: Option<&hew_types::check::CheckedSelectSource>,
    ) -> HirSelectArmKind {
        use hew_types::check::CheckedSelectSource;
        // The checker refuses `await` in an arm source; a malformed arm still
        // reaches here behind an error type, so unwrap the operand rather than
        // key on a span the checker never recorded.
        let operand = match &source.0 {
            Expr::Await(inner) => inner.as_ref(),
            _ => source,
        };
        let key = self.mk_key(&operand.1);
        match checked {
            Some(CheckedSelectSource::TaskAwait {
                operand: checked_key,
            }) if checked_key == &key => {
                return HirSelectArmKind::TaskAwait {
                    task: Box::new(self.lower_expr(operand, IntentKind::Read)),
                };
            }
            Some(CheckedSelectSource::ActorAsk { call }) if call == &key => {
                return HirSelectArmKind::ActorAsk {
                    call: Box::new(self.lower_expr(operand, IntentKind::Read)),
                };
            }
            Some(CheckedSelectSource::StreamReceive { call }) if call == &key => {
                if let Expr::MethodCall { receiver, .. } = &operand.0 {
                    return HirSelectArmKind::StreamNext {
                        stream: Box::new(self.lower_expr(receiver, IntentKind::Read)),
                    };
                }
            }
            _ => {}
        }
        self.diagnostics.push(HirDiagnostic::new(
            HirDiagnosticKind::CheckerBoundaryViolation {
                name: "select source".to_string(),
                reason: "missing or inconsistent checked source-arm classification".to_string(),
            },
            source.1.clone(),
            "select source requires checker-owned classification",
        ));
        HirSelectArmKind::TaskAwait {
            task: Box::new(
                self.unsupported_expr(source.1.clone(), "invalid checked select source"),
            ),
        }
    }
}
