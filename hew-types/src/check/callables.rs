//! Binding-resolved callable construction and invocation contracts.

use super::{
    Checker, ClosureCaptureFact, Expr, HashSet, MarkerTrait, ResolvedTy, Span, Spanned, Ty,
    TypeError, TypeErrorKind,
};
use crate::env::{TypeBindingId, TypeEnv};
use crate::{
    CallableCallMode, CallableCapabilities, ClosureCaptureAccess, ClosureCaptureAcquisition,
    ClosureCaptureConsumption,
};

impl Checker {
    pub(super) fn resolve_private_captures(
        &mut self,
        captures: &[Spanned<String>],
    ) -> HashSet<TypeBindingId> {
        let mut bindings = HashSet::new();
        for (name, span) in captures {
            if let Some(binding) = self.env.lookup_ref(name) {
                if !bindings.insert(binding.id) {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!("duplicate private capture `{name}`"),
                    );
                }
            } else {
                self.report_error(
                    TypeErrorKind::UndefinedVariable,
                    span,
                    format!("private capture `{name}` must name an existing binding"),
                );
            }
        }
        bindings
    }

    pub(super) fn finish_closure_captures(
        &mut self,
        facts: Vec<ClosureCaptureFact>,
        private: &HashSet<TypeBindingId>,
        body: &TypeEnv,
        is_move: bool,
        span: &Span,
    ) -> Vec<ClosureCaptureFact> {
        let mut seen = HashSet::new();
        let mut captures = Vec::new();
        for mut fact in facts {
            if !seen.insert(fact.binding_id) {
                continue;
            }
            fact.ty = self.subst.resolve(&fact.ty).materialize_literal_defaults();
            fact.acquisition = if is_move {
                ClosureCaptureAcquisition::Move
            } else {
                ClosureCaptureAcquisition::Snapshot
            };
            fact.access = if private.contains(&fact.binding_id) {
                ClosureCaptureAccess::Var
            } else {
                ClosureCaptureAccess::Read
            };
            fact.consumption = if body.binding_by_id(fact.binding_id).is_some_and(|binding| {
                binding.capture_consumption == ClosureCaptureConsumption::Consumed
            }) {
                ClosureCaptureConsumption::Consumed
            } else {
                ClosureCaptureConsumption::Retained
            };
            fact.is_send = self.registry.implements_marker(&fact.ty, MarkerTrait::Send);
            fact.is_sync = self.registry.is_sync(&fact.ty);
            let is_copy = self.registry.implements_marker(&fact.ty, MarkerTrait::Copy);
            if is_move {
                if !is_copy {
                    self.env.mark_moved(&fact.name, span.clone());
                }
            } else if !self.capture_is_cloneable(&fact.ty)
                && !matches!(fact.ty, Ty::Error | Ty::Var(_))
            {
                self.report_error(
                    TypeErrorKind::ClosureExplicitMoveRequired {
                        name: fact.name.clone(), ty: fact.ty.user_facing().to_string(),
                    }, &fact.use_span,
                    format!("capture `{}` has no independent snapshot operation; use `move` to transfer it into the closure", fact.name),
                );
            }
            captures.push(fact);
        }
        captures
    }

    pub(super) fn closure_capabilities(
        &self,
        captures: &[ClosureCaptureFact],
    ) -> CallableCapabilities {
        let call = if captures
            .iter()
            .any(|fact| fact.consumption == ClosureCaptureConsumption::Consumed)
        {
            CallableCallMode::Once
        } else if captures
            .iter()
            .any(|fact| fact.access == ClosureCaptureAccess::Var)
        {
            CallableCallMode::Var
        } else {
            CallableCallMode::Read
        };
        CallableCapabilities {
            call,
            clone: captures
                .iter()
                .all(|fact| self.capture_is_cloneable(&fact.ty)),
        }
    }
    fn capture_is_cloneable(&self, ty: &Ty) -> bool {
        let Ok(resolved) = ResolvedTy::from_ty(ty) else {
            return false;
        };
        let declarations = self.class_declarations();
        let context = crate::value_class::ClassContext::new(&declarations);
        matches!(
            crate::value_class::ValueClass::of_ty(&resolved, &context),
            Ok(crate::value_class::ValueClass::BitCopy
                | crate::value_class::ValueClass::CowValue
                | crate::value_class::ValueClass::PersistentShare
                | crate::value_class::ValueClass::View)
        ) && (self.registry.implements_marker(ty, MarkerTrait::Copy)
            || self.registry.implements_marker(ty, MarkerTrait::Clone))
    }

    pub(super) fn private_capture_mutation_error(
        &self,
        name: &str,
        span: &Span,
    ) -> Option<TypeError> {
        let depth = self.lambda_capture_depth?;
        let (binding_depth, binding) = self.env.lookup_ref_with_depth(name)?;
        if binding_depth >= depth || binding.is_mutable || self.in_lambda_actor_body {
            return None;
        }
        Some(TypeError::new(TypeErrorKind::MutabilityError, span.clone(),
            format!("capture `{name}` is an immutable snapshot; add `[var {name}]` before the lambda to mutate its private field")))
    }

    pub(super) fn check_callable_receiver(&mut self, ty: &Ty, callee: &Spanned<Expr>) {
        let (Ty::Function { capabilities, .. } | Ty::Closure { capabilities, .. }) =
            self.subst.resolve(ty)
        else {
            return;
        };
        let place = self.expr_place(&callee.0);
        if let Some((root, path)) = &place {
            if self
                .env
                .lookup_ref(root)
                .is_some_and(|binding| binding.is_moved)
            {
                self.report_error(
                    TypeErrorKind::UseAfterMove,
                    &callee.1,
                    format!("cannot invoke consumed callable `{root}`"),
                );
            } else {
                self.report_place_use_after_move(root, path, &callee.1);
            }
        }
        match capabilities.call {
            CallableCallMode::Read => {}
            CallableCallMode::Var => {
                let writable = place.as_ref().is_some_and(|(root, _)| {
                    self.env
                        .lookup_ref(root)
                        .is_some_and(|binding| binding.is_mutable)
                });
                if writable {
                    if let Some((root, _)) = place {
                        self.env.mark_written(&root);
                    }
                } else {
                    let error = place.as_ref().and_then(|(root, _)| self.private_capture_mutation_error(root, &callee.1))
                        .unwrap_or_else(|| TypeError::new(TypeErrorKind::MutabilityError, callee.1.clone(),
                            "this callable requires a mutable place; bind it with `var` before calling"));
                    self.errors.push(error);
                }
            }
            CallableCallMode::Once => self.mark_expr_moved(&callee.0, &callee.1),
        }
    }
}
