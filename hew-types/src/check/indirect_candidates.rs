//! Checker-owned provenance for function values at indirect calls.

use super::scope::Resolution;
use super::types::{
    CallableArgumentFlow, CallableCandidate, CallableDispatchActual, CallableFieldFlow, Checker,
    IndirectCallCandidates, PendingCallableArguments, PendingCallableTarget, SpanKey,
};
use super::{CallTarget, MethodCallRewrite};
use crate::DeclarationKind;
use hew_parser::ast::{CallArg, Expr, Ident, Span, Spanned};

impl Checker {
    /// Read the selected declaration or lexical binding at the authored
    /// expression. An absent row is opaque; a spelling is never a candidate.
    fn resolved_callable_candidate(&self, span: &Span) -> IndirectCallCandidates {
        let key = SpanKey::in_module(span, self.current_module_idx);
        match self.scopes.resolutions().get(&key) {
            Some(Resolution::Local(id)) => self
                .env
                .value_candidates(*id)
                .cloned()
                .unwrap_or_else(IndirectCallCandidates::unknown),
            Some(Resolution::Def(id) | Resolution::Member(id))
                if matches!(
                    self.defs.kind(*id),
                    DeclarationKind::Function
                        | DeclarationKind::ExternFunction
                        | DeclarationKind::TraitMethod
                        | DeclarationKind::TypeMethod
                        | DeclarationKind::ImplMethod
                        | DeclarationKind::DefaultImplMethod
                ) =>
            {
                IndirectCallCandidates::single(CallableCandidate::Declaration(*id))
            }
            _ => IndirectCallCandidates::unknown(),
        }
    }

    pub(super) fn callable_candidates_for_expr(
        &self,
        expr: &Expr,
        span: &Span,
    ) -> IndirectCallCandidates {
        if let Some(candidates) = self
            .expression_value_candidates
            .get(&SpanKey::in_module(span, self.current_module_idx))
        {
            return candidates.clone();
        }

        match expr {
            Expr::ForkChild { .. } | Expr::ForkBlock { .. } | Expr::Race(_) => {
                IndirectCallCandidates::single(CallableCandidate::TaskProducer(SpanKey::in_module(
                    span,
                    self.current_module_idx,
                )))
            }
            Expr::Await(value) => self
                .callable_candidates_for_expr(&value.0, &value.1)
                .task_results(),
            Expr::Tuple(values) => self.callable_sequence_candidates(values.iter()),
            Expr::Array(values) => self.callable_sequence_candidates(
                values.iter().map(hew_parser::ast::ArrayElement::expr),
            ),
            Expr::Lambda { .. } | Expr::SpawnLambdaActor { .. } => IndirectCallCandidates::single(
                CallableCandidate::Closure(SpanKey::in_module(span, self.current_module_idx)),
            ),
            Expr::Ident(_) => self.resolved_callable_candidate(span),
            Expr::FieldAccess { object, field } => self.callable_field_candidates(object, field),
            Expr::GenericApplySuffix { target, .. } => {
                self.callable_candidates_for_expr(&target.0, &target.1)
            }
            Expr::ContextVariant(context) if context.record.is_some() => {
                IndirectCallCandidates::single(CallableCandidate::Aggregate(SpanKey::in_module(
                    span,
                    self.current_module_idx,
                )))
            }
            Expr::StructInit { .. } => IndirectCallCandidates::single(
                CallableCandidate::Aggregate(SpanKey::in_module(span, self.current_module_idx)),
            ),
            Expr::Call { args, .. }
                if matches!(
                    self.actor_delivery_calls
                        .get(&SpanKey::in_module(span, self.current_module_idx)),
                    Some(crate::actor_delivery::ActorDeliveryCall::Policy { .. })
                ) =>
            {
                args.first()
                    .map_or_else(IndirectCallCandidates::unknown, |target| {
                        let (value, value_span) = target.expr();
                        self.callable_candidates_for_expr(value, value_span)
                    })
            }
            Expr::Call { args, .. } | Expr::MethodCall { args, .. }
                if self.is_construct_call(span) =>
            {
                self.callable_sequence_candidates(args.iter().map(CallArg::expr))
            }
            Expr::Call { .. } | Expr::MethodCall { .. } => IndirectCallCandidates::single(
                CallableCandidate::CallResult(SpanKey::in_module(span, self.current_module_idx)),
            ),
            Expr::If { .. } | Expr::IfLet { .. } | Expr::Match { .. } => {
                self.branch_value_candidates(expr)
            }
            Expr::Block(block)
            | Expr::Scope { body: block }
            | Expr::ScopeDeadline { body: block, .. } => self.callable_candidates_for_block(block),
            Expr::UnsafeBlock(block) => self.callable_candidates_for_block(block),
            Expr::Coalesce { left, right } => {
                let mut candidates = self.callable_candidates_for_expr(&left.0, &left.1);
                candidates.join(self.callable_candidates_for_expr(&right.0, &right.1));
                candidates
            }
            Expr::Clone(value)
            | Expr::Cast { expr: value, .. }
            | Expr::PostfixTry(value)
            | Expr::Index { object: value, .. }
            | Expr::ArrayRepeat { value, .. } => {
                self.callable_candidates_for_expr(&value.0, &value.1)
            }
            _ => IndirectCallCandidates::unknown(),
        }
    }

    fn branch_value_candidates(&self, expr: &Expr) -> IndirectCallCandidates {
        match expr {
            Expr::If {
                then_block,
                else_block: Some(else_block),
                ..
            } => {
                let mut candidates =
                    self.callable_candidates_for_expr(&then_block.0, &then_block.1);
                candidates.join(self.callable_candidates_for_expr(&else_block.0, &else_block.1));
                candidates
            }
            Expr::IfLet {
                body,
                else_body: Some(else_body),
                ..
            } => {
                let mut candidates = self.callable_candidates_for_block(body);
                candidates.join(self.callable_candidates_for_expr(&else_body.0, &else_body.1));
                candidates
            }
            Expr::Match { arms, .. } if !arms.is_empty() => {
                let mut arms = arms.iter();
                let first = arms.next().expect("nonempty match arms");
                let mut candidates =
                    self.callable_candidates_for_expr(&first.body.0, &first.body.1);
                for arm in arms {
                    candidates.join(self.callable_candidates_for_expr(&arm.body.0, &arm.body.1));
                }
                candidates
            }
            _ => IndirectCallCandidates::unknown(),
        }
    }

    fn callable_field_candidates(
        &self,
        object: &Spanned<Expr>,
        field: &Spanned<Ident>,
    ) -> IndirectCallCandidates {
        let key = SpanKey::in_module(&field.1, self.current_module_idx);
        if let Some(Resolution::Field(owner, index)) = self.scopes.resolutions().get(&key) {
            let receiver = self.callable_candidates_for_expr(&object.0, &object.1);
            IndirectCallCandidates {
                known: receiver
                    .known
                    .into_iter()
                    .map(|receiver| CallableCandidate::Field {
                        receiver: Box::new(receiver),
                        owner: *owner,
                        index: *index,
                    })
                    .collect(),
                may_be_unknown: receiver.may_be_unknown,
            }
        } else {
            self.resolved_callable_candidate(&field.1)
        }
    }

    fn callable_sequence_candidates<'a>(
        &self,
        values: impl Iterator<Item = &'a Spanned<Expr>>,
    ) -> IndirectCallCandidates {
        IndirectCallCandidates::single(CallableCandidate::Sequence(
            values
                .map(|value| self.callable_candidates_for_expr(&value.0, &value.1))
                .collect(),
        ))
    }

    pub(super) fn callable_candidates_for_block(
        &self,
        block: &hew_parser::ast::Block,
    ) -> IndirectCallCandidates {
        block
            .trailing_expr
            .as_ref()
            .map_or_else(IndirectCallCandidates::unknown, |value| {
                self.callable_candidates_for_expr(&value.0, &value.1)
            })
    }

    pub(super) fn record_binding_value_candidates(
        &mut self,
        name: Ident,
        value: Option<&Spanned<Expr>>,
    ) {
        let Some(binding) = self.env.lookup_ref(name.name.as_str()) else {
            return;
        };
        let Some(value) = value else {
            return;
        };
        let candidates = self.callable_candidates_for_expr(&value.0, &value.1);
        let id = binding.id;
        self.env.set_value_candidates(id, candidates);
    }

    pub(super) fn record_assigned_value_candidates(
        &mut self,
        target: &Spanned<Expr>,
        value: &Spanned<Expr>,
    ) {
        let candidates = self.callable_candidates_for_expr(&value.0, &value.1);
        self.replace_place_value_candidates(target, candidates);
    }

    fn current_place_value_candidates(&self, place: &Spanned<Expr>) -> IndirectCallCandidates {
        let key = SpanKey::in_module(&place.1, self.current_module_idx);
        if let Some(Resolution::Local(binding)) = self.scopes.resolutions().get(&key) {
            return self
                .env
                .value_candidates(*binding)
                .cloned()
                .unwrap_or_else(IndirectCallCandidates::unknown);
        }
        if let Expr::FieldAccess { object, field } = &place.0 {
            let key = SpanKey::in_module(&field.1, self.current_module_idx);
            if let Some(Resolution::Field(owner, index)) = self.scopes.resolutions().get(&key) {
                let source = self.current_place_value_candidates(object);
                return IndirectCallCandidates {
                    known: source
                        .known
                        .into_iter()
                        .map(|receiver| CallableCandidate::Field {
                            receiver: Box::new(receiver),
                            owner: *owner,
                            index: *index,
                        })
                        .collect(),
                    may_be_unknown: source.may_be_unknown,
                };
            }
        }
        self.callable_candidates_for_expr(&place.0, &place.1)
    }

    fn replace_place_value_candidates(
        &mut self,
        target: &Spanned<Expr>,
        candidates: IndirectCallCandidates,
    ) {
        let key = SpanKey::in_module(&target.1, self.current_module_idx);
        match &target.0 {
            Expr::Ident(_) => {
                if let Some(Resolution::Local(binding)) = self.scopes.resolutions().get(&key) {
                    self.env.set_value_candidates(*binding, candidates);
                }
            }
            Expr::FieldAccess { object, field } => {
                let field_key = SpanKey::in_module(&field.1, self.current_module_idx);
                let Some(Resolution::Field(owner, selected)) =
                    self.scopes.resolutions().get(&field_key).copied()
                else {
                    return;
                };
                let object_key = SpanKey::in_module(&object.1, self.current_module_idx);
                let Some(ty) = self.expr_types.get(&object_key) else {
                    return;
                };
                let Some(definition) = self.ty_type_def(&self.subst.resolve(ty)) else {
                    return;
                };
                let count = definition.field_order.len();
                let base = self.current_place_value_candidates(object);
                let fields = (0..count)
                    .map(|index| {
                        let index = u32::try_from(index).expect("checked field index");
                        CallableFieldFlow {
                            owner,
                            index,
                            candidates: if index == selected {
                                candidates.clone()
                            } else {
                                IndirectCallCandidates {
                                    known: base
                                        .known
                                        .iter()
                                        .map(|receiver| CallableCandidate::Field {
                                            receiver: Box::new(receiver.clone()),
                                            owner,
                                            index,
                                        })
                                        .collect(),
                                    may_be_unknown: base.may_be_unknown,
                                }
                            },
                        }
                    })
                    .collect();
                self.aggregate_field_candidates.insert(key.clone(), fields);
                self.replace_place_value_candidates(
                    object,
                    IndirectCallCandidates::single(CallableCandidate::Aggregate(key)),
                );
            }
            _ => {}
        }
    }

    pub(super) fn record_indirect_call_candidates(&mut self, span: &Span, callee: &Spanned<Expr>) {
        let candidates = self.callable_candidates_for_expr(&callee.0, &callee.1);
        self.indirect_call_candidates.insert(
            SpanKey::in_module(span, self.current_module_idx),
            candidates,
        );
    }

    pub(super) fn record_unknown_indirect_call_candidates(&mut self, span: &Span) {
        self.indirect_call_candidates.insert(
            SpanKey::in_module(span, self.current_module_idx),
            IndirectCallCandidates::unknown(),
        );
    }

    /// Parameters retain their own symbolic origin until a selected call
    /// supplies an actual value. This remains precise through helper-to-helper
    /// forwarding without globally merging unrelated callers.
    pub(super) fn record_callable_formal_candidate(&mut self, name: Ident) {
        let Some(binding) = self.env.lookup_ref(name.name.as_str()) else {
            return;
        };
        let id = binding.id;
        self.env.set_value_candidates(
            id,
            IndirectCallCandidates::single(CallableCandidate::Formal(id)),
        );
    }

    fn callable_target_declaration(target: &CallTarget) -> Option<crate::DefId> {
        match target {
            CallTarget::User(id) | CallTarget::ImplMethod(id) => Some(*id),
            CallTarget::Extern { declaration, .. } => Some(*declaration),
            _ => None,
        }
    }

    fn selected_callable_target(&self, span: &Span) -> Option<&CallTarget> {
        let key = SpanKey::in_module(span, self.current_module_idx);
        self.method_call_rewrites
            .get(&key)
            .and_then(|rewrite| match rewrite {
                MethodCallRewrite::RewriteToFunction { target, .. }
                | MethodCallRewrite::RewriteModuleQualifiedToFunction { target, .. }
                | MethodCallRewrite::StaticTraitDispatch { target, .. }
                | MethodCallRewrite::BinderStaticCall(super::types::BinderTraitCall {
                    target,
                    ..
                }) => Some(target),
                _ => None,
            })
            .or_else(|| self.direct_call_targets.get(&key))
            .or_else(|| self.resolved_calls.get(&key).map(|call| &call.target))
    }

    fn selected_callable_declaration(&self, span: &Span) -> Option<crate::DefId> {
        self.selected_callable_target(span)
            .and_then(Self::callable_target_declaration)
    }

    /// Capture authored actuals after checking selected this call's exact
    /// declaration. Formal IDs may be registered later in source order; the
    /// completed table joins them at the output boundary.
    pub(super) fn record_call_argument_sources(&mut self, expr: &Expr, span: &Span) {
        let (receiver, args): (Option<&Spanned<Expr>>, &[CallArg]) = match expr {
            Expr::Call { args, .. } => (None, args),
            Expr::MethodCall { receiver, args, .. } => (Some(receiver), args),
            _ => return,
        };
        let key = SpanKey::in_module(span, self.current_module_idx);
        // `T.make(n)` names its binder, not a receiver value.
        let receiver = receiver.filter(|_| {
            !matches!(
                self.method_call_rewrites.get(&key),
                Some(MethodCallRewrite::BinderStaticCall(_))
            )
        });
        if matches!(
            self.selected_callable_target(span),
            Some(CallTarget::StaticTraitMethod { .. })
        ) {
            let receiver_offset = usize::from(receiver.is_some());
            let mut actuals = Vec::with_capacity(args.len() + receiver_offset);
            if let Some(receiver) = receiver {
                actuals.push(CallableDispatchActual {
                    slot: 0,
                    candidates: self.callable_candidates_for_expr(&receiver.0, &receiver.1),
                });
            }
            let slots = self.call_argument_slots.get(&key);
            for (index, arg) in args.iter().enumerate() {
                let (value, value_span) = arg.expr();
                actuals.push(CallableDispatchActual {
                    slot: slots.map_or(index, |slots| slots[index]) + receiver_offset,
                    candidates: self.callable_candidates_for_expr(value, value_span),
                });
            }
            self.generic_trait_call_arguments.insert(key, actuals);
            return;
        }
        let callee = if let Some(declaration) = self.selected_callable_declaration(span) {
            PendingCallableTarget::Declaration(declaration)
        } else if let Expr::Call { function, .. } = expr {
            PendingCallableTarget::Indirect(
                self.callable_candidates_for_expr(&function.0, &function.1),
            )
        } else {
            return;
        };
        let receiver = receiver.map(|value| self.callable_candidates_for_expr(&value.0, &value.1));
        let arguments = args
            .iter()
            .map(|arg| {
                let (value, value_span) = arg.expr();
                self.callable_candidates_for_expr(value, value_span)
            })
            .collect();
        self.pending_callable_arguments.insert(
            key,
            PendingCallableArguments {
                callee,
                receiver,
                arguments,
            },
        );
    }

    /// The initializer's written labels already carry checker-selected field
    /// identities. Preserve their value origins under the constructor site.
    pub(super) fn record_aggregate_field_sources(&mut self, expr: &Expr, span: &Span) {
        let (fields, field_labels) = match expr {
            Expr::StructInit {
                fields,
                field_labels,
                ..
            } => (fields, field_labels),
            Expr::ContextVariant(context) => {
                let Some(record) = context.record.as_ref() else {
                    return;
                };
                (&record.fields, &record.field_labels)
            }
            _ => return,
        };
        let mut writes = Vec::new();
        for ((_, value), label) in fields.iter().zip(field_labels) {
            let key = SpanKey::in_module(&label.span, self.current_module_idx);
            let field = if label.shorthand {
                self.scopes.shorthand_label(&key)
            } else {
                match self.scopes.resolutions().get(&key) {
                    Some(Resolution::Field(owner, index)) => Some((*owner, *index)),
                    _ => None,
                }
            };
            let Some((owner, index)) = field else {
                continue;
            };
            writes.push(CallableFieldFlow {
                owner,
                index,
                candidates: self.callable_candidates_for_expr(&value.0, &value.1),
            });
        }
        self.aggregate_field_candidates
            .insert(SpanKey::in_module(span, self.current_module_idx), writes);
    }

    /// Return expressions are interpreted under the caller's actual-to-formal
    /// environment. Keep their symbolic origin rather than joining callers.
    pub(super) fn record_callable_body_return(
        &mut self,
        declaration: crate::DefId,
        body: &hew_parser::ast::Block,
    ) {
        let Some(tail) = &body.trailing_expr else {
            return;
        };
        let candidates = self.callable_candidates_for_expr(&tail.0, &tail.1);
        self.callable_return_candidates
            .entry(super::effects::EffectBody::Declaration(declaration))
            .and_modify(|existing| existing.join(candidates.clone()))
            .or_insert(candidates);
    }

    pub(super) fn finish_callable_argument_flows(
        &mut self,
    ) -> std::collections::HashMap<SpanKey, Vec<CallableArgumentFlow>> {
        let mut flows = std::collections::HashMap::new();
        for (site, pending) in self.pending_callable_arguments.clone() {
            let PendingCallableTarget::Declaration(callee) = pending.callee else {
                continue;
            };
            let Some(formals) = self
                .callable_formals
                .get(&super::effects::EffectBody::Declaration(callee))
            else {
                continue;
            };
            let receiver_offset = usize::from(
                pending.receiver.is_some() && formals.len() == pending.arguments.len() + 1,
            );
            if formals.len() != pending.arguments.len() + receiver_offset {
                continue;
            }
            let mut actuals = Vec::with_capacity(formals.len());
            if receiver_offset == 1 {
                actuals.push(CallableArgumentFlow {
                    callee,
                    formal: formals[0],
                    candidates: pending
                        .receiver
                        .expect("receiver offset requires a receiver"),
                });
            }
            let slots = self.call_argument_slots.get(&site);
            for (index, candidates) in pending.arguments.into_iter().enumerate() {
                let slot = slots.map_or(index, |slots| slots[index]) + receiver_offset;
                let Some(formal) = formals.get(slot) else {
                    actuals.clear();
                    break;
                };
                actuals.push(CallableArgumentFlow {
                    callee,
                    formal: *formal,
                    candidates,
                });
            }
            if !actuals.is_empty() {
                flows.insert(site, actuals);
            }
        }
        flows
    }
}
