//! Mutable method calls use ordinary callable boundaries and canonical places.

use super::{Builder, PreparedCallee};
use crate::{
    BoundaryDecision, BoundaryOperand, Operand, OwnKind, Provenance, SemOpKind, SemParamPassing,
    ValueId,
};
use hew_hir::{HirExpr, HirExprKind, IntentKind};
use hew_types::{CallTarget, ResolvedTy};

impl Builder<'_, '_> {
    #[allow(
        clippy::too_many_lines,
        reason = "receiver acquisition and normal writeback share one exact source place"
    )]
    pub(super) fn lower_var_self_call(&mut self, expr: &HirExpr) -> Result<ValueId, String> {
        let HirExprKind::VarSelfMethodCall {
            receiver,
            receiver_update,
            call_target,
            args,
            ret_ty,
            receiver_ty,
            ..
        } = &expr.kind
        else {
            return Err("mutable method lowering requires a var-self call".into());
        };
        let receiver_ty = self.ty(receiver_ty);
        let callee = match call_target {
            CallTarget::User(declaration) | CallTarget::ImplMethod(declaration) => self
                .service
                .resolve_direct_call(declaration, expr.site, &self.substitution)?,
            // A generic body reaches `it.next()` through its bound; the
            // implementation is selected here, from the receiver type this
            // instance substituted.
            CallTarget::StaticTraitMethod {
                declaring_trait,
                method,
            } => self.service.resolve_static_trait_call(
                declaring_trait,
                method,
                &receiver_ty,
                expr.site,
                &self.substitution,
            )?,
            _ => {
                return Err(
                    "mutable method requires a resolved direct or static-trait declaration".into(),
                )
            }
        };
        let source = self
            .service
            .table
            .functions_by_item
            .get(&callee.function)
            .ok_or_else(|| "mutable method has no HIR source declaration".to_string())?;
        if source
            .params
            .first()
            .is_none_or(|param| source.var_self_receiver != Some(param.id))
        {
            return Err("mutable method target has no exact var-self receiver contract".into());
        }
        let return_ty = self.ty(ret_ty);
        let signature = callee.signature;
        let dual_return_ty = ResolvedTy::Tuple(vec![return_ty.clone(), receiver_ty.clone()]);
        if receiver.intent != IntentKind::Consume
            || self.ty(&receiver.ty) != receiver_ty
            || self.ty(&expr.ty) != return_ty
            || signature.return_ty != dual_return_ty
            || signature.params.len() != args.len() + 1
            || signature.params[0].ty != receiver_ty
        {
            return Err(format!(
                "mutable method `{}` differs from its checked receiver and dual-return signature: receiver {receiver_ty:?}, result {return_ty:?}, signature {signature:?}",
                callee.declaration.full_path()
            ));
        }
        self.service.require_type_facts(&receiver_ty)?;
        if *receiver_update == hew_types::ReceiverUpdate::Staged
            && self
                .service
                .checked_facts
                .rows()
                .get(&hew_types::TypeInstanceKey(receiver_ty.clone()))
                .is_none_or(|facts| facts.clone == hew_types::CloneKind::None)
        {
            return Err("staged mutable receiver requires an independent value copy".into());
        }
        let owns_receiver =
            OwnKind::of_ty(&receiver_ty, self.service.checked_facts.rows())? == OwnKind::Owned;
        let passing = if owns_receiver {
            SemParamPassing::Consume
        } else {
            SemParamPassing::ReadOnly
        };
        if signature.params[0].passing != passing {
            return Err(
                "mutable method receiver passing differs from its exact ownership contract".into(),
            );
        }
        let place = self.resolve_mutable_place(receiver)?;
        let provenance = Provenance::Site(expr.site);
        let live_before_arguments = self.owned_live.keys().copied().collect();
        let mut loans = Vec::new();
        let mut arguments = self.lower_user_arguments(args, &signature.params[1..], &mut loans)?;

        // Later arguments may replace this binding or one of its sibling fields.
        // Acquire its current value only after those effects have completed.
        let selected = self.owned_projection(&place)?;
        let (value, scalar_parents) = if let Some(selected) = selected {
            let (root, _) = crate::projection::place_path(&self.places, selected)?;
            self.snapshot_arguments_rooted_at(root, &mut arguments, &mut loans, &provenance)?;
            let value = self.emit_typed(
                provenance.clone(),
                &receiver_ty,
                if owns_receiver && *receiver_update == hew_types::ReceiverUpdate::Replace {
                    SemOpKind::LoadTake { place: selected }
                } else {
                    SemOpKind::LoadCopy { place: selected }
                },
            )?;
            (value, Vec::new())
        } else {
            if owns_receiver {
                return Err("mutable method receiver has no owning place".into());
            }
            self.take_scalar_place(&place, &provenance)?
        };
        let value = if owns_receiver {
            self.owned_live.remove(&value);
            self.emit_typed(
                provenance.clone(),
                &receiver_ty,
                SemOpKind::Move {
                    source: Operand { value },
                },
            )?
        } else {
            value
        };
        arguments.insert(
            0,
            BoundaryOperand {
                operand: Operand { value },
                decision: if owns_receiver {
                    BoundaryDecision::Move
                } else {
                    BoundaryDecision::Copy
                },
            },
        );
        let result = self
            .finish_user_call(
                PreparedCallee::Direct(callee.id),
                signature,
                arguments,
                &loans,
                &live_before_arguments,
                true,
            )?
            .ok_or_else(|| "mutable method did not return its result and receiver".to_string())?;
        let shape = self.service.require_aggregate_shape(&dual_return_ty)?;
        let fields =
            self.emit_destructure_value(result, &dual_return_ty, shape, provenance.clone())?;
        if let Some(selected) = selected {
            self.store_projected(selected, fields[1].id, provenance)?;
        } else {
            self.replace_scalar_aggregate_leaf(
                place.binding,
                fields[1].id,
                scalar_parents,
                &provenance,
            )?;
        }
        Ok(fields[0].id)
    }
}
