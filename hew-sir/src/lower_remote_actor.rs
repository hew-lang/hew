//! Authenticated peer acquisition yields the actor's ordinary reference type.

use crate::SirLoweringError;

use super::Builder;
use crate::ValueId;
use hew_hir::HirExpr;
use hew_types::ResolvedTy;

impl Builder<'_, '_> {
    pub(super) fn lower_service_acquire(
        &mut self,
        expression: &HirExpr,
        operation: hew_types::runtime_call::ServiceOp,
        args: &[&HirExpr],
    ) -> Result<Option<ValueId>, SirLoweringError> {
        let output = self.ty(&expression.ty);
        let ResolvedTy::Named { args: result, .. } = &output else {
            return Err("service acquisition lacks Result".into());
        };
        let export = operation == hew_types::runtime_call::ServiceOp::Export;
        let success = result.first().ok_or(SirLoweringError::from(
            "service acquisition lacks its actor",
        ))?;
        let actor_ty = if export {
            let ResolvedTy::Named { args, .. } = success else {
                return Err("service export lacks its actor".into());
            };
            let [actor] = args.as_slice() else {
                return Err("service export names one actor".into());
            };
            actor
        } else {
            success
        };
        let actor = self.service.require_actor(actor_ty)?;
        let descriptor = &self.service.actors[actor.0 as usize];
        if !descriptor
            .handlers
            .iter()
            .any(|handler| handler.codec.is_some())
        {
            return Err("service acquisition requires at least one wire handler".into());
        }
        if export
            && args
                .get(1)
                .is_none_or(|argument| self.ty(&argument.ty) != *actor_ty)
        {
            return Err("service export requires its exact local actor handle".into());
        }
        self.lower_borrowed_native_suspend(
            expression,
            crate::SuspendKind::ServiceAcquire { actor, export },
            operation.row().contract.expect("service contract"),
            args,
        )
    }
}
