//! Checker-owned provenance for function values at indirect calls.

use super::scope::Resolution;
use super::types::{
    CallableArgumentFlow, CallableCandidate, Checker, IndirectCallCandidates,
    PendingCallableArguments, SpanKey,
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
                .callable_binding_candidates
                .get(id)
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

    fn callable_candidates_for_expr(&self, expr: &Expr, span: &Span) -> IndirectCallCandidates {
        match expr {
            Expr::Lambda { .. } => IndirectCallCandidates::single(CallableCandidate::Closure(
                SpanKey::in_module(span, self.current_module_idx),
            )),
            Expr::Ident(_) => self.resolved_callable_candidate(span),
            Expr::FieldAccess { field, .. } => self.resolved_callable_candidate(&field.1),
            Expr::GenericApplySuffix { target, .. } => {
                self.callable_candidates_for_expr(&target.0, &target.1)
            }
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
            Expr::Block(block) => self.callable_candidates_for_block(block),
            Expr::Coalesce { left, right } => {
                let mut candidates = self.callable_candidates_for_expr(&left.0, &left.1);
                candidates.join(self.callable_candidates_for_expr(&right.0, &right.1));
                candidates
            }
            Expr::Clone(value) | Expr::Cast { expr: value, .. } => {
                self.callable_candidates_for_expr(&value.0, &value.1)
            }
            _ => IndirectCallCandidates::unknown(),
        }
    }

    fn callable_candidates_for_block(
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

    pub(super) fn record_callable_binding_candidates(
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
        self.callable_binding_candidates
            .insert(binding.id, candidates);
    }

    pub(super) fn join_assigned_callable_candidates(
        &mut self,
        target: &Spanned<Expr>,
        value: &Spanned<Expr>,
    ) {
        let Expr::Ident(name) = &target.0 else {
            return;
        };
        let Some(binding) = self.env.lookup_ref(name.name.as_str()) else {
            return;
        };
        let id = binding.id;
        let value_candidates = self.callable_candidates_for_expr(&value.0, &value.1);
        self.callable_binding_candidates
            .entry(id)
            .or_insert_with(IndirectCallCandidates::unknown)
            .join(value_candidates);
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
        self.callable_binding_candidates.insert(
            binding.id,
            IndirectCallCandidates::single(CallableCandidate::Formal(binding.id)),
        );
    }

    fn callable_target_declaration(target: &CallTarget) -> Option<crate::DefId> {
        match target {
            CallTarget::User(id) | CallTarget::ImplMethod(id) => Some(*id),
            CallTarget::Extern { declaration, .. } => Some(*declaration),
            _ => None,
        }
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
        let target = self
            .method_call_rewrites
            .get(&key)
            .and_then(|rewrite| match rewrite {
                MethodCallRewrite::RewriteToFunction { target, .. }
                | MethodCallRewrite::RewriteModuleQualifiedToFunction { target, .. }
                | MethodCallRewrite::StaticTraitDispatch { target, .. } => Some(target),
                _ => None,
            })
            .or_else(|| self.direct_call_targets.get(&key));
        let Some(callee) = target.and_then(Self::callable_target_declaration) else {
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

    pub(super) fn finish_callable_argument_flows(
        &mut self,
    ) -> std::collections::HashMap<SpanKey, Vec<CallableArgumentFlow>> {
        let mut flows = std::collections::HashMap::new();
        for (site, pending) in std::mem::take(&mut self.pending_callable_arguments) {
            let Some(formals) = self.callable_formals.get(&pending.callee) else {
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
                    callee: pending.callee,
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
                    callee: pending.callee,
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
