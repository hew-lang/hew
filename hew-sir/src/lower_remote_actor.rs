//! `RemotePid` send and ask address the actor's checked remote member.

use super::{lower_initial_value_transfer, Builder, OwnedBindingUse};
use crate::{BlockArg, CallResult, Edge, Operand, OwnKind, SemTerminator, ValueDef, ValueId};
use hew_hir::{HirExpr, HirExprKind};
use hew_types::ResolvedTy;

impl Builder<'_, '_> {
    /// The actor a `RemotePid<A>` names and its member with wire codecs.
    fn remote_member(&mut self, receiver: &HirExpr) -> Result<(crate::ActorId, u32), String> {
        let ResolvedTy::Named {
            head: hew_types::TypeHead::Builtin(hew_types::BuiltinType::RemotePid),
            args,
            ..
        } = self.ty(&receiver.ty)
        else {
            return Err("remote call receiver is not a RemotePid".into());
        };
        let [actor_ty] = args.as_slice() else {
            return Err("RemotePid names exactly one actor".into());
        };
        let actor = self.service.require_actor(actor_ty)?;
        let message = self.service.actors[actor.0 as usize]
            .handlers
            .iter()
            .find(|handler| handler.codec.is_some())
            .ok_or("remote actor lacks its checked remote member")?
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
        let HirExprKind::RemoteActorSend { receiver, msg } = &expression.kind else {
            unreachable!()
        };
        let (actor, message) = self.remote_member(receiver)?;
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
            msg,
            timeout_ms,
            ..
        } = &expression.kind
        else {
            unreachable!()
        };
        let (actor, message) = self.remote_member(receiver)?;
        let output = self.ty(&expression.ty);
        let signature = self.service.actors[actor.0 as usize].remote_signature(
            message,
            &self.ty(&receiver.ty),
            output.clone(),
            true,
        )?;
        self.service.require_type_facts(&output)?;
        let (target, value) = match self.remote_request(receiver, msg, &signature.params[1].ty)? {
            Ok(request) => request,
            Err(diverged) => return Ok(diverged),
        };
        let timeout = self.lower_expr(timeout_ms)?;
        if !self.is_open() {
            return Ok(timeout);
        }
        self.owned_live.remove(&value);
        let inputs = vec![
            crate::BoundaryOperand {
                operand: Operand { value: target },
                decision: crate::BoundaryDecision::Borrow,
            },
            crate::BoundaryOperand {
                operand: Operand { value },
                decision: crate::BoundaryDecision::Move,
            },
            crate::BoundaryOperand {
                operand: Operand { value: timeout },
                decision: crate::BoundaryDecision::Copy,
            },
        ];
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
        let saved = self.control_state();
        for cleanup in [cancel, unwind] {
            self.restore_control_state(&saved);
            self.current = cleanup;
            self.finish_fault_exit()?;
        }
        self.restore_control_state(&saved);
        self.current = resumed;
        if own == OwnKind::Owned {
            self.owned_live.insert(result, output);
        }
        Ok(result)
    }
}
