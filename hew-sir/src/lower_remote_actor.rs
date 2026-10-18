//! `RemotePid` send and ask address the actor's checked remote member.

use super::{lower_initial_value_transfer, Builder, OwnedBindingUse};
use crate::{BlockArg, CallResult, Edge, Operand, OwnKind, SemTerminator, ValueDef, ValueId};
use hew_hir::{HirExpr, HirExprKind};
use hew_types::ResolvedTy;

impl Builder<'_, '_> {
    pub(super) fn lower_service_acquire(
        &mut self,
        expression: &HirExpr,
        operation: hew_types::runtime_call::ServiceOp,
        args: &[&HirExpr],
    ) -> Result<Option<ValueId>, String> {
        let output = self.ty(&expression.ty);
        let ResolvedTy::Named { args: result, .. } = &output else {
            return Err("service acquisition lacks Result".into());
        };
        let export = operation == hew_types::runtime_call::ServiceOp::Export;
        let success = result
            .first()
            .ok_or("service acquisition lacks its actor")?;
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

    fn remote_member(
        &mut self,
        receiver: &HirExpr,
        member: hew_types::actor_protocol::RemoteActorMember,
    ) -> Result<(crate::ActorId, u32), String> {
        let ResolvedTy::Named { args, .. } = self.ty(&receiver.ty) else {
            return Err("remote call receiver lacks its actor interface".into());
        };
        let [actor_ty] = args.as_slice() else {
            return Err("remote call names exactly one actor".into());
        };
        let actor = self.service.require_actor(actor_ty)?;
        let descriptor = &self.service.actors[actor.0 as usize];
        let declaration = match member {
            hew_types::actor_protocol::RemoteActorMember::Declared(declaration) => declaration,
            hew_types::actor_protocol::RemoteActorMember::ActorMsg => {
                let ResolvedTy::Named { args, .. } = actor_ty else {
                    unreachable!()
                };
                let source = self
                    .service
                    .module
                    .items
                    .iter()
                    .find_map(|item| match item {
                        hew_hir::HirItem::Actor(source)
                            if source.declaration == descriptor.declaration =>
                        {
                            Some(source)
                        }
                        _ => None,
                    })
                    .ok_or("remote actor lacks its checked source protocol")?;
                let selected = source
                    .protocol_descriptor
                    .handlers
                    .iter()
                    .filter(|handler| {
                        handler.remote_receivers.iter().any(|receiver| {
                            receiver.interface
                                == hew_types::actor_protocol::RemoteActorInterface::Message
                                && receiver.matches(args)
                        })
                    })
                    .collect::<Vec<_>>();
                let [selected] = selected.as_slice() else {
                    return Err("ActorMsg must select one exact receive member".into());
                };
                selected.declaration
            }
        };
        let message = descriptor
            .handlers
            .iter()
            .find(|handler| handler.declaration == declaration && handler.codec.is_some())
            .ok_or("remote call lacks its exact portable receive declaration")?
            .message_id;
        Ok((actor, message))
    }

    /// Evaluate the pid, then the message converted to the member's payload.
    /// `Err` carries the value that closed a diverged evaluation.
    fn remote_request(
        &mut self,
        receiver: &HirExpr,
        msg: &HirExpr,
        payload: &ResolvedTy,
    ) -> Result<Result<(ValueId, ValueId), ValueId>, String> {
        let target =
            lower_initial_value_transfer(self, receiver, "remote target", OwnedBindingUse::Copy)?;
        if !self.is_open() {
            return Ok(Err(target));
        }
        let value =
            lower_initial_value_transfer(self, msg, "remote message", OwnedBindingUse::Copy)?;
        if !self.is_open() {
            return Ok(Err(value));
        }
        let value = self.coerce_value(value, payload, crate::Provenance::Site(msg.site))?;
        Ok(Ok((target, value)))
    }

    pub(super) fn lower_remote_actor_send(
        &mut self,
        expression: &HirExpr,
    ) -> Result<ValueId, String> {
        let HirExprKind::RemoteActorSend {
            receiver,
            msg,
            member,
        } = &expression.kind
        else {
            unreachable!()
        };
        let (actor, message) = self.remote_member(receiver, *member)?;
        let operation = crate::ActorOperation::RemoteSend {
            actor,
            message,
            target: self.ty(&receiver.ty),
        };
        let signature = self.actor_signature(&operation)?;
        if signature.return_ty != self.ty(&expression.ty) {
            return Err("remote send answers with its i32 submission status".into());
        }
        let (target, value) = match self.remote_request(receiver, msg, &signature.params[1].ty)? {
            Ok(request) => request,
            Err(diverged) => return Ok(diverged),
        };
        self.emit_actor_call(operation, signature, vec![target, value])?
            .ok_or_else(|| "remote send lacks its submission status".into())
    }

    /// The request is encoded before the caller parks, so the message owner
    /// transfers at the suspension; cancellation and faults own nothing else.
    pub(super) fn lower_remote_actor_ask(
        &mut self,
        expression: &HirExpr,
    ) -> Result<ValueId, String> {
        let HirExprKind::RemoteActorAsk {
            receiver,
            member,
            args,
            evaluation_order,
            timeout_ms,
            ..
        } = &expression.kind
        else {
            unreachable!()
        };
        let (actor, message) = self.remote_member(receiver, *member)?;
        let output = self.ty(&expression.ty);
        let signature = self.service.actors[actor.0 as usize].remote_signature(
            &self.service.module.defs,
            message,
            &self.ty(&receiver.ty),
            output.clone(),
            true,
        )?;
        self.service.require_type_facts(&output)?;
        let before: std::collections::HashSet<_> = self.owned_live.keys().copied().collect();
        let mut loans = Vec::new();
        let target = self
            .lower_call_read(receiver, &mut loans, true, true)?
            .value;
        if !self.is_open() {
            return Ok(target);
        }
        let parameters = signature
            .params
            .iter()
            .skip(1)
            .take(args.len())
            .map(|parameter| parameter.ty.clone())
            .collect::<Vec<_>>();
        let values = match self.lower_actor_payload(args, &parameters, evaluation_order)? {
            std::ops::ControlFlow::Continue(values) => values,
            std::ops::ControlFlow::Break(value) => return Ok(value),
        };
        let timeout = self.lower_expr(timeout_ms)?;
        if !self.is_open() {
            return Ok(timeout);
        }
        let mut inputs = vec![crate::BoundaryOperand {
            operand: Operand { value: target },
            decision: crate::BoundaryDecision::Borrow,
        }];
        for value in values {
            self.owned_live.remove(&value);
            inputs.push(crate::BoundaryOperand {
                operand: Operand { value },
                decision: crate::BoundaryDecision::Move,
            });
        }
        inputs.push(crate::BoundaryOperand {
            operand: Operand { value: timeout },
            decision: crate::BoundaryDecision::Copy,
        });
        let own = OwnKind::of_ty(&output, self.service.checked_facts.rows())?;
        let raw = self.fresh_value();
        let result = self.fresh_value();
        let resumed = self.new_block(vec![BlockArg {
            value: result,
            ty: output.clone(),
            own,
        }]);
        let cancel = self.new_block(Vec::new());
        let unwind = self.new_block(Vec::new());
        let edge = |target| Edge {
            target,
            args: Vec::new(),
        };
        self.set_terminator(SemTerminator::Suspend {
            kind: crate::SuspendKind::RemoteAsk { actor, message },
            inputs,
            result: CallResult::Value(ValueDef {
                id: raw,
                ty: output.clone(),
                own,
            }),
            resumes: vec![Edge {
                target: resumed,
                args: vec![Operand { value: raw }],
            }],
            cancel: edge(cancel),
            unwind: edge(unwind),
        })?;
        self.finish_remote_reply(
            resumed,
            cancel,
            unwind,
            &loans,
            &before,
            ValueDef {
                id: result,
                ty: output,
                own,
            },
        )
    }

    fn finish_remote_reply(
        &mut self,
        normal: crate::BlockId,
        cancel: crate::BlockId,
        unwind: crate::BlockId,
        loans: &[ValueId],
        before: &std::collections::HashSet<ValueId>,
        result: ValueDef,
    ) -> Result<ValueId, String> {
        let saved = self.control_state();
        for cleanup in [cancel, unwind] {
            self.restore_control_state(&saved);
            self.current = cleanup;
            self.end_call_loans(loans)?;
            self.finish_fault_exit()?;
        }
        self.restore_control_state(&saved);
        self.current = normal;
        self.end_call_loans(loans)?;
        let temporaries = self
            .owned_live
            .keys()
            .filter(|value| !before.contains(value))
            .copied()
            .collect::<Vec<_>>();
        for temporary in temporaries.into_iter().rev() {
            self.emit_destroy(temporary)?;
        }
        if result.own == OwnKind::Owned {
            self.owned_live.insert(result.id, result.ty);
        }
        Ok(result.id)
    }
}
